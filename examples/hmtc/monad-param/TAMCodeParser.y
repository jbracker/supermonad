-- ***************************************************************************
-- *                               H M T C                                   *
-- *                                                                         *
-- *    Module:         TAMCodeParser                                        *
-- *    Purpose:        Parser for TAM Code                                  *
-- *    Authors:        Henrik Nilsson                                       *
-- *                                                                         *
-- *               Copyright (c) Henrik Nilsson, 2006 - 2013                 *
-- *                                                                         *
-- ***************************************************************************

{
-- | Parser for TAM code

module TAMCodeParser (
    parseTC             -- :: String -> DF [TAMInst]
) where

-- Standard library imports
import Data.Char (isDigit, isAlpha, isAlphaNum)

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import Type (MTInt, isMTInt)
import ParseMonad
import TAMCode
import PPTAMCode

}

----------------------------------------------------------------
-- TAM Code Parser
----------------------------------------------------------------

-- Happy grammar with semantic actions for building list of TAM instructions.
-- Convention: Terminals are either written in upper case or within
-- single quotes. Non-terminals are written in lower case.

%name parseAux

%monad { P } { >>= } { return }

%lexer { scanner } { (T_EOF, _) }

-- The terminal symbols are tokens paired with a source code position.
%tokentype { (Token, SrcPos) }

-- The semantic values of constant terminal symbols are the associated
-- source code position. The semantic value of terminal symbols that
-- carry additional information (like identifiers) is the token and
-- source code position pair itself.
%token
    '['         { (T_LBrk, $$) }
    ']'         { (T_RBrk, $$) }
    ':'         { (T_Colon, $$) }
    '+'         { (T_Plus, $$) }
    '-'         { (T_Minus, $$) }
    SB          { (T_SB, $$) }
    LB          { (T_LB, $$) }
    ST          { (T_ST, $$) }
    LOADL       { (T_LOADL, $$) }
    LOADCA      { (T_LOADCA, $$) }
    LOAD        { (T_LOAD, $$) }
    LOADA       { (T_LOADA, $$) }
    LOADI       { (T_LOADI, $$) }
    STORE       { (T_STORE, $$) }
    STOREI      { (T_STOREI, $$) }
    LOADLB      { (T_LOADLB, $$) }
    LOADIB      { (T_LOADIB, $$) }
    STOREIB     { (T_STOREIB, $$) }
    POP         { (T_POP, $$) }
    ADD         { (T_ADD, $$) }
    SUB         { (T_SUB, $$) }
    MUL         { (T_MUL, $$) }
    DIV         { (T_DIV, $$) }
    NEG         { (T_NEG, $$) }
    LSS         { (T_LSS, $$) }
    EQL         { (T_EQL, $$) }
    GTR         { (T_GTR, $$) }
    AND         { (T_AND, $$) }
    OR          { (T_OR, $$) }
    NOT         { (T_NOT, $$) }
    JUMP        { (T_JUMP, $$) }
    JUMPIFZ     { (T_JUMPIFZ, $$) }
    JUMPIFNZ    { (T_JUMPIFNZ, $$) }
    CALL        { (T_CALL, $$) }
    CALLI       { (T_CALLI, $$) }
    RETURN      { (T_RETURN, $$) }
    PUTINT      { (T_PUTINT, $$) }
    PUTCHR      { (T_PUTCHR, $$) }
    GETINT      { (T_GETINT, $$) }
    GETCHR      { (T_GETCHR, $$) }
    HALT        { (T_HALT, $$) }
    LITINT      { (T_LitInt {}, _) }
    ID          { (T_Id {}, _) }

%%

program :: { [TAMInst] }
program : lines                         { reverse $1 }

lines :: { [TAMInst] }
lines : {- epsilon -}                   { [] }
      | lines labeldef                  { $2 : $1 }
      | lines instruction               { $2 : $1 }


labeldef :: { TAMInst }
labeldef : label ':'                    { Label $1 }


instruction :: { TAMInst }
instruction : LOADL     litint          { LOADL $2 }
            | LOADCA    label           { LOADCA $2 }
            | LOAD      addr            { LOAD $2 }
            | LOADA     addr            { LOADA $2 }
            | LOADI     disp            { LOADI $2 }
            | STORE     addr            { STORE $2 }
            | STOREI    disp            { STOREI $2 }
            | LOADLB    litint size     { LOADLB $2 $3 }
            | LOADIB    size            { LOADIB $2 }
            | STOREIB   size            { STOREIB $2 }
            | POP       size size       { POP $2 $3 }
            | ADD                       { ADD }
            | SUB                       { SUB }
            | MUL                       { MUL }
            | DIV                       { DIV }
            | NEG                       { NEG }
            | LSS                       { LSS }
            | EQL                       { EQL }
            | GTR                       { GTR }
            | AND                       { AND }
            | OR                        { OR  }
            | NOT                       { NOT }
            | JUMP      label           { JUMP $2 }
            | JUMPIFZ   label           { JUMPIFZ $2 }
            | JUMPIFNZ  label           { JUMPIFNZ $2 }
            | CALL      label           { CALL $2 }
            | CALLI                     { CALLI }
            | RETURN    size size       { RETURN $2 $3}
            | PUTINT                    { PUTINT }
            | PUTCHR                    { PUTCHR }
            | GETINT                    { GETINT }
            | GETCHR                    { GETCHR }
            | HALT                      { HALT }


label :: { Name }
label : ID                              { tspIdName $1 }


addr :: { Addr }
addr : '[' SB slitint ']'               { SB $3 }
     | '[' LB slitint ']'               { LB $3 }
     | '[' ST slitint ']'               { ST $3 }


disp :: { MTInt }
disp : litint                           { $1 }


size :: { MTInt }
size : ulitint                          { $1 }


litint :: { MTInt }
litint : ulitint                        { $1 }
       | slitint                        { $1 }


slitint :: { MTInt }
slitint : '+' LITINT                    {% toMTInt (tspLIVal $2) }
        | '-' LITINT                    {% toMTInt (-(tspLIVal $2)) }


ulitint :: { MTInt }
ulitint : LITINT                        {% toMTInt (tspLIVal $1) }


{

happyError :: P a
happyError = failP "Parse error"


toMTInt :: Integer -> P MTInt
toMTInt n =
    if isMTInt n then
        return (fromInteger n)
    else
        failP ("The integer " ++ show n
               ++ " is outside the MiniTriangle range.")


-- | Parses a TAM program, building a list of TAM instructions if successful.

parseTC :: String -> DF [TAMInst]
parseTC = runP parseAux


-- Token type for TAM code

data Token
    -- Graphical tokens
    = T_LBrk      -- "["
    | T_RBrk      -- "]"
    | T_Colon     -- ":"
    | T_Plus      -- "+"
    | T_Minus     -- "-"

    -- Registers
    | T_SB
    | T_LB
    | T_ST

    -- Instructions
    | T_LOADL
    | T_LOADCA
    | T_LOAD
    | T_LOADA
    | T_LOADI
    | T_STORE
    | T_STOREI
    | T_LOADLB
    | T_LOADIB
    | T_STOREIB
    | T_POP
    | T_ADD
    | T_SUB
    | T_MUL
    | T_DIV
    | T_NEG
    | T_LSS
    | T_EQL
    | T_GTR
    | T_AND
    | T_OR
    | T_NOT
    | T_JUMP
    | T_JUMPIFZ
    | T_JUMPIFNZ
    | T_CALL
    | T_CALLI
    | T_RETURN
    | T_PUTINT
    | T_PUTCHR
    | T_GETINT
    | T_GETCHR
    | T_HALT

    -- Tokens with variable spellings
    | T_LitInt {liVal :: Integer}
    | T_Id     {idName :: Name}

    -- End Of File marker
    | T_EOF
    deriving (Eq, Show)


-- Projection functions for pairs of token and source position.

tspSrcPos :: (Token,SrcPos) -> SrcPos
tspSrcPos = snd


tspLIVal :: (Token,SrcPos) -> Integer
tspLIVal (T_LitInt {liVal = n}, _) = n
tspLIVal _ = parserErr "tspLIVal" "Not a LitInt"


tspIdName :: (Token,SrcPos) -> Name
tspIdName (T_Id {idName = nm}, _) = nm
tspIdName _ = parserErr "tspIdName" "Not an Id"


tabWidth :: Int
tabWidth = 8


nextTabStop :: Int -> Int
nextTabStop n = n + (tabWidth - (n-1) `mod` tabWidth)


-- TAM code scanner

scanner :: ((Token, SrcPos) -> P a) -> P a
scanner cont = P $ scan
    where
        -- scan :: Int -> Int -> String -> D a
        -- End Of File
        scan l c []         = retTkn T_EOF l c c []
        -- Skip white space and comments, including handling various
        -- line ending conventions (NL, CR+NL, NL+CR) gracefully.
        scan l c ('\n' : s) = scan (l + 1) 1 s
        scan l c ('\r' : s) = scan l 1 s
        scan l c ('\t' : s) = scan l (nextTabStop c) s
        scan l c (' ' : s)  = scan l (c + 1) s
        scan l c (';' : s)  = scan l c (dropWhile (/='\n') s)
        -- Scan graphical tokens
        scan l c ('[' : s)  = retTkn T_LBrk l c (c + 1) s
        scan l c (']' : s)  = retTkn T_RBrk l c (c + 1) s
        scan l c (':' : s)  = retTkn T_Colon l c (c + 1) s
        scan l c ('+' : s)  = retTkn T_Plus l c (c + 1) s
        scan l c ('-' : s)  = retTkn T_Minus l c (c + 1) s
        -- Scan numeric literals, dentifiers, and instructions
        scan l c (x : s) | isDigit x      = scanLitInt l c x s
                         | isIdStartChr x = scanIdOrKwd l c x s
                         | otherwise = do
                                emitErrD (SrcPos l c)
                                         ("Lexical error: Illegal character "
                                           ++ show x  ++ " (discarded)")
                                scan l (c + 1) s

        -- scanLitInt :: Int -> Int -> Char -> String -> D a
        -- Note: We cannot check that the scanned integer literal fits
        -- in an MTInt already here as that would rule out -2^31.
        scanLitInt l c x s = retTkn (T_LitInt (read (x : tail))) l c c' s'
            where
                (tail, s') = span isDigit s
                c'         = c + 1 + length tail

        -- scanIdOrKwd :: Int -> Int -> Char -> String -> D a
        scanIdOrKwd l c x s = retTkn (mkIdOrKwd (x : tail)) l c c' s'
            where
                (tail, s') = span isIdChr s
                c'         = c + 1 + length tail

        mkIdOrKwd :: String -> Token
        mkIdOrKwd "SB"       = T_SB
        mkIdOrKwd "LB"       = T_LB
        mkIdOrKwd "ST"       = T_ST
        mkIdOrKwd "LOADL"    = T_LOADL
        mkIdOrKwd "LOADCA"   = T_LOADCA
        mkIdOrKwd "LOAD"     = T_LOAD
        mkIdOrKwd "LOADA"    = T_LOADA
        mkIdOrKwd "LOADI"    = T_LOADI
        mkIdOrKwd "STORE"    = T_STORE
        mkIdOrKwd "STOREI"   = T_STOREI
        mkIdOrKwd "LOADLB"   = T_LOADLB
        mkIdOrKwd "LOADIB"   = T_LOADIB
        mkIdOrKwd "STOREIB"  = T_STOREIB
        mkIdOrKwd "POP"      = T_POP
        mkIdOrKwd "ADD"      = T_ADD
        mkIdOrKwd "SUB"      = T_SUB
        mkIdOrKwd "MUL"      = T_MUL
        mkIdOrKwd "DIV"      = T_DIV
        mkIdOrKwd "NEG"      = T_NEG
        mkIdOrKwd "LSS"      = T_LSS
        mkIdOrKwd "EQL"      = T_EQL
        mkIdOrKwd "GTR"      = T_GTR
        mkIdOrKwd "AND"      = T_AND
        mkIdOrKwd "OR"       = T_OR
        mkIdOrKwd "NOT"      = T_NOT
        mkIdOrKwd "JUMP"     = T_JUMP
        mkIdOrKwd "JUMPIFZ"  = T_JUMPIFZ
        mkIdOrKwd "JUMPIFNZ" = T_JUMPIFNZ
        mkIdOrKwd "CALL"     = T_CALL
        mkIdOrKwd "CALLI"    = T_CALLI
        mkIdOrKwd "RETURN"   = T_RETURN
        mkIdOrKwd "PUTINT"   = T_PUTINT
        mkIdOrKwd "PUTCHR"   = T_PUTCHR
        mkIdOrKwd "GETINT"   = T_GETINT
        mkIdOrKwd "GETCHR"   = T_GETCHR
        mkIdOrKwd "HALT"     = T_HALT
        mkIdOrKwd name       = T_Id {idName = name}

        -- Return token, position of token, updated position, and remaning
        -- input. We assume tnat no MiniTriangle token span multiple
        -- lines. Hence only one line number argument is needed.
        -- retTkn :: Token -> Int -> Int -> Int -> String -> D a
        retTkn t l c c' = unP (cont (t, SrcPos {spLine = l, spCol = c})) l c'


isIdChr :: Char -> Bool
isIdChr c = isAlphaNum c || c == '#' || c == '$' || c == '@' || c == '_'


isIdStartChr :: Char -> Bool
isIdStartChr c = isAlpha c || c == '#' || c == '$' || c == '@' || c == '_'


-- Test utility. Attempts to parse the given string input and,
-- if successful, pretty-prints the resulting AST.

testTCParser :: String -> IO ()
testTCParser s = do
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) (snd result)
    putStrLn ""
    case fst result of
        Just is -> do
            putStrLn "TAM Code:"
            putStrLn (ppTAMCode is)
        Nothing ->
            putStrLn "Parsing produced no result."
    putStrLn ""
    where
        result :: (Maybe [TAMInst], [DMsg])
        result = runDF (parseTC s)


-- Internal error reporting

parserErr :: String -> String -> a
parserErr = internalError "TAMCodeParser"

}
