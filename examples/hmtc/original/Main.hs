{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Main                                                 *
*       Purpose:        Main MiniTriangle compiler driver.                   *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2014                  *
*                                                                            *
******************************************************************************
-}

-- | Main MiniTriangle compiler driver.

module Main (main) where

-- Standard library imports
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import System.Environment (getArgs)

-- HMTC module imports
import SrcPos (SrcPos(..))
import Diagnostics
import Token (Token)
import AST (AST)
import PPAST
import MTIR (MTIR)
import PPMTIR
import TAMCode (TAMInst)
import TAMCodeParser (parseTC)
import PPTAMCode (ppTAMCode)
import Scanner
import Parser
import TypeChecker
import CodeGenerator
import TAMInterpreter
import LibMT

version :: String
version = "Haskell Mini Triangle Compiler (HMTC) version 2.00 (Complete)"


------------------------------------------------------------------------------
-- Options and File Type
------------------------------------------------------------------------------

data Options =
    Options {
        optHelp       :: Bool,
        optSAScanning :: Bool,
        optPAScanning :: Bool,
        optSAParsing  :: Bool,
        optPAParsing  :: Bool,
        optSAChecking :: Bool,
        optPAChecking :: Bool,
        optSACodeGen  :: Bool,
        optPACodeGen  :: Bool,
        optPeepOpt    :: Bool,
        optRun        :: Bool,
        optTrace      :: Bool,
        optVersion    :: Bool
    }
    deriving Show


defaultOptions :: Options
defaultOptions =
    Options {
        optHelp       = False,
        optSAScanning = False,
        optPAScanning = False,
        optSAParsing  = False,
        optPAParsing  = False,
        optSAChecking = False,
        optPAChecking = False,
        optSACodeGen  = False,
        optPACodeGen  = False,
        optPeepOpt    = True,
        optRun        = False,
        optTrace      = False,
        optVersion    = False
    }


data FileType = MT | TAM

-- Leading path and base name.
-- Crude (will not always work), but good enough for this compiler.
baseName = takeWhile (/='.')

-- Crude (will not always work), but good enough for this compiler.
fileExt  = dropWhile (/='.')


fileType :: String -> FileType
fileType fn = if fileExt fn == ".tam" then TAM else MT


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

-- | Usage (command line):
--
--      [hmtc \[options\] file.mt]      Compile \"file.mt\".
--
--      [hmtc \[options\]]              Read program to compile from standard
--                                      input.
--
--      [hmtc \[options\] file.tam]     Run TAM code \"file.tam\". Use option
--                                      \"--run-traced\" to turn on tracing.
--
-- Options:
--
--      [--help]                        Print help message and stop.
--
--      [--stop-after-scanning]         Stop after scanning.
--                                      Implies \"--print-after-scanning\".
--
--      [--print-after-scanning]        Print intermediate representation after
--                                      scanning.
--
--      [--stop-after-parsing]          Stop after parsing.
--                                      Implies \"--print-after-parsing\".
--
--      [--print-after-parsing]         Print intermediate representation after
--                                      parsing.
--
--      [--stop-after-checking]         Stop after type checking.
--                                      Implies \"--print-after-checking\".
--
--      [--print-after-checking]        Print intermediate representation after
--                                      type checking.
--
--      [--stop-after-codegen]          Stop after code generation.
--                                      Implies \"--print-after-codegen\".
--
--      [--print-after-codegen]         Print generated TAM code.
--
--      [--peepopt]                     Do peephole optimization on the
--                                      generated TAM code (default).
--
--      [--no-peepopt]                  Do not do peephole optimization on the
--                                      generated TAM code.
--
--      [--run]                         Interpret generated TAM code.
--
--      [--run-traced]                  Interpret generated TAM code and trace
--                                      the execution.
--
--      [--version]                     Print HMTC version and stop.

main :: IO ()
main = do
    (opts, mfn) <- parseCmdLine
    if optHelp opts then
        printHelp
     else if optVersion opts then
        putStrLn version
     else do
        (ft, prog) <- case mfn of
                          Nothing -> do
                              prog <- getContents
                              return (MT, prog)
                          Just fn  -> do
                              prog <- readFile fn
                              return (fileType fn, prog)
        case ft of
            TAM -> do
                let (mbCode, msgs) = runDF (parseTC prog)
                mapM_ (putStrLn . ppDMsg) msgs
                case mbCode of
                    Nothing -> putStrLn "No valid TAM code read."
                    Just code ->
                        runTAM (optTrace opts) (code ++ libMT)
            MT -> do
                let (mbCode, msgs) = runDF (compile opts prog)
                mapM_ (putStrLn . ppDMsg) msgs
                case mbCode of
                    Nothing   -> putStrLn "No code generated."
                    Just code ->
                        if optRun opts then
                            runTAM (optTrace opts) (code ++ libMT)
                        else
                            case mfn of
                                Nothing -> putStr (ppTAMCode code)
                                Just fn  -> do
                                    let fn' = baseName fn ++ ".tam"
                                    writeFile fn' (ppTAMCode code)
                                    putStrLn ("Code written to file \""
                                              ++ fn' ++ "\"")


------------------------------------------------------------------------------
-- Parse the command line
------------------------------------------------------------------------------

parseCmdLine :: IO (Options, Maybe String)
parseCmdLine = do
    args <- getArgs
    let (mof, msgs) = runDF (processOptions defaultOptions args)
    mapM_ (putStrLn . ppDMsg) msgs
    case mof of
        Just (opts, as) -> return (opts,
                                   if null as then
                                       Nothing
                                   else
                                       (Just (head as)))
        Nothing -> ioError (userError "Aborted.")


processOptions :: Options -> [String] -> DF (Options, [String])
processOptions opts as = do
    oas <- dToDF (posAux opts as)
    failIfErrorsD
    return oas
    where
        posAux :: Options -> [String] -> D (Options, [String])
        posAux opts [] = return (opts, [])
        posAux opts aas@(a:as)
            | take 2 a /= "--" = return (opts, aas)
            | otherwise        = do
                opts' <- poAux opts (drop 2 a)
                posAux opts' as

        poAux :: Options -> String -> D Options
        poAux opts o
            | o == "help" =
                return (opts {optHelp = True})
            | o == "stop-after-scanning" =
                return (opts {optSAScanning = True, optPAScanning = True})
            | o == "print-after-scanning" =
                return (opts {optPAScanning = True})
            | o == "stop-after-parsing" =
                return (opts {optSAParsing = True, optPAParsing = True})
            | o == "print-after-parsing" =
                return (opts {optPAParsing = True})
            | o == "stop-after-checking" =
                return (opts {optSAChecking = True, optPAChecking = True})
            | o == "print-after-checking" =
                return (opts {optPAChecking = True})
            | o == "stop-after-codegen" =
                return (opts {optSACodeGen = True, optPACodeGen = True})
            | o == "print-after-codegen" =
                return (opts {optPACodeGen = True})
            | o == "peepopt" =
                return (opts {optPeepOpt = True})
            | o == "no-peepopt" =
                return (opts {optPeepOpt = False})
            | o == "run" =
                return (opts {optRun = True, optTrace = False})
            | o == "run-traced" =
                return (opts {optRun = True, optTrace = True})
            | o == "version" =
                return (opts {optVersion = True})
            | otherwise = do
                emitErrD NoSrcPos ("Unknown option \"--" ++ o ++ "\"")
                return opts


------------------------------------------------------------------------------
-- Compiler
------------------------------------------------------------------------------

compile :: Options -> String -> DF [TAMInst]
compile opts src = do
    -- Scanning
    -- The scanner and parser operate in a tightly interleaved fashion. Thus
    -- to see the result of scanning only, we need to invoke the scanner
    -- separately. (The result of scanning is then discarded.)
    when (optPAScanning opts) $ do
        tss <- scan src
        emitInfoD NoSrcPos (ppTSs tss)
        failIfErrorsD
    when (optSAScanning opts) stopD

    -- Parsing
    ast <- parse src
    when (optPAParsing opts) (emitInfoD NoSrcPos (ppAST ast))
    failIfErrorsD
    when (optSAParsing opts) stopD

    -- Type checking
    mtir <- dToDF (typeCheck ast)
    when (optPAChecking opts) (emitInfoD NoSrcPos (ppMTIR mtir))
    failIfErrorsD
    when (optSAChecking opts) stopD

    -- Code generation
    code <- dToDF (genCode (optPeepOpt opts) mtir)
    when (optPACodeGen opts) (emitInfoD NoSrcPos (ppTAMCode code))
    failIfErrorsD
    when (optSACodeGen opts) stopD

    return code

    where
        ppTSs :: [(Token, SrcPos)] -> String
        ppTSs tss = (foldr (.)
                           id
                           (map (\ts -> shows ts . showChar '\n') tss)) ""


------------------------------------------------------------------------------
-- Print Help Text
------------------------------------------------------------------------------

helpText = "\
\Usage:\n\
\    hmtc [options] file.mt      Compile \"file.mt\"\n\
\    hmtc [options]              Read program to compile from standard\n\
\                                input.\n\
\    hmtc [options] file.tam     Run TAM code \"file.tam\". Use option\n\
\                                \"--run-traced\" to turn on tracing.\n\
\Options:\n\
\    --help                      Print help message and stop.\n\
\    --stop-after-scanning       Stop after scanning.\n\
\                                Implies \"--print-after-scanning\".\n\
\    --print-after-scanning      Print intermediate representation after\n\
\                                scanning.\n\
\    --stop-after-parsing        Stop after parsing.\n\
\                                Implies \"--print-after-parsing\".\n\
\    --print-after-parsing       Print intermediate representation after\n\
\                                parsing.\n\
\    --stop-after-checking       Stop after type checking.\n\
\                                Implies \"--print-after-checking\".\n\
\    --print-after-checking      Print intermediate representation after\n\
\                                type checking.\n\
\    --stop-after-codegen        Stop after code generation.\n\
\                                Implies \"--print-after-codegen\".\n\
\    --print-after-codegen       Print generated TAM code.\n\
\    --peepopt                   Do peephole optimization on the generated\n\
\                                TAM code (default).\n\
\    --no-peepopt                Do not do peephole optimization on the\n\
\                                generated TAM code.\n\
\    --run                       Interpret generated TAM code.\n\
\    --run-traced                Interpret generated TAM code and trace\n\
\                                the execution.\n\
\    --version                   Print HMTC version and stop.\n\
\"

printHelp :: IO ()
printHelp = putStr helpText
