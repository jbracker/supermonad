
-- | Log message formatting and debuging functions.
module Control.Super.Plugin.Log
  ( pprToStr, sDocToStr
  , missingCaseError
  , smErrMsg, smDebugMsg, smObjMsg, smWarnMsg
  , formatGroupSrcSpans
  , formatConstraint, formatSpan
  -- * Debug Functions
  , printTrace, printObjTrace, trace
  -- * Debugging and priniting from within TcPluginM
  , printObj, printMsg, printErr, printWarn
  , pluginAssert, pluginFailSDoc
  ) where

import Data.List ( groupBy, intercalate )

import Debug.Trace ( trace )

import SrcLoc
  ( SrcSpan(..)
  , srcSpanFileName_maybe
  , srcSpanStartLine, srcSpanEndLine
  , srcSpanStartCol, srcSpanEndCol )
import Outputable ( Outputable, SDoc )
import FastString ( unpackFS )
import TcRnTypes
  ( Ct(..), CtFlavour(..)--, CtLoc(..)
  , ctFlavour, ctPred )
import TcPluginM ( TcPluginM, tcPluginIO, unsafeTcPluginTcM )
import IOEnv ( failWithM )

import Control.Super.Plugin.Debug ( pprToStr, sDocToStr )
import Control.Super.Plugin.Utils ( removeDup )
import Control.Super.Plugin.Constraint ( constraintSourceLocation )

-- | @prefixMsg prefix msg@ prefixes a message with the given string.
prefixMsg :: String -> String -> String
prefixMsg prefix = unlines . fmap ((pluginMsgPrefix ++ prefix) ++) . lines

-- | Message prefix of the plugin.
pluginMsgPrefix :: String
pluginMsgPrefix = "[SM]"

-- | Prefix a message with the error prefix.
smErrMsg :: String -> String
smErrMsg = prefixMsg $ " ERROR: "

-- | Prefix a message with the warning prefix.
smWarnMsg :: String -> String
smWarnMsg = prefixMsg $ " WARNING: "

-- | Prefix a message with the standard debug prefix.
smDebugMsg :: String -> String
smDebugMsg = prefixMsg $ " "

-- | Prefix a message with the debug prefix and a note that this is a
--   printed object.
smObjMsg :: String -> String
smObjMsg = prefixMsg $ "> "

-- | Used to emit an error with a message describing the missing case.
--   The string is the function that misses the case and the 'Outputable'
--   is the object being matched.
missingCaseError :: (Outputable o) => String -> Maybe o -> a
missingCaseError funName (Just val) = error $ "Missing case in '" ++ funName ++ "' for " ++ pprToStr val
missingCaseError funName Nothing    = error $ "Missing case in '" ++ funName ++ "'"

-- -----------------------------------------------------------------------------
-- Formatting
-- -----------------------------------------------------------------------------

-- | Format a list of source spans by grouping spans in the same file together.
formatGroupSrcSpans :: [SrcSpan] -> String
formatGroupSrcSpans spans = unwords $ fmap formatSpanGroup groupedSpans
  where
    formatSpanGroup :: [SrcSpan] -> String
    formatSpanGroup [] = ""
    formatSpanGroup ss@(s:_) =
      case srcSpanFileName_maybe s of
        Nothing -> intercalate ", " $ fmap formatSpan ss
        Just file -> unpackFS file ++ ": " ++ intercalate ", " (fmap formatSpan ss) ++ ";"

    groupedSpans = groupBy eqFileName $ removeDup spans
    eqFileName s1 s2 = srcSpanFileName_maybe s1 == srcSpanFileName_maybe s2

-- | Format a source span.
formatSpan :: SrcSpan -> String
formatSpan (UnhelpfulSpan str) = unpackFS str
formatSpan (RealSrcSpan s) =
  show (srcSpanStartLine s) ++ ":" ++
  show (srcSpanStartCol s) ++ "-" ++
  (if srcSpanStartLine s /= srcSpanEndLine s then show (srcSpanEndLine s) ++ ":" else "") ++
  show (srcSpanEndCol s)

-- | Format a constraint in a readable way, without displaying information
--   irrelevant to the plugin.
--
--   /Example:/
--
-- >>> [G] Supermonad m Identity m (129:12-131:41, CDictCan)
-- >>> [D] m_a1kdW ~ m (134:3-14, CNonCanonical)
formatConstraint :: Ct -> String
formatConstraint ct
  =  "["  ++ formatCtFlavour ct
  ++ "] " ++ formatCtType ct
  ++ " (" ++ formatSpan (constraintSourceLocation ct)
  ++ ", " ++ formatCtDataCon ct
  ++ ")"
  where
    formatCtDataCon :: Ct -> String
    formatCtDataCon c = case c of
      CDictCan      {} -> "CDictCan"
      CIrredEvCan   {} -> "CIrredEvCan"
      CTyEqCan      {} -> "CTyEqCan"
      CFunEqCan     {} -> "CFunEqCan"
      CNonCanonical {} -> "CNonCanonical"
      CHoleCan      {} -> "CHoleCan"
    formatCtFlavour :: Ct -> String
    formatCtFlavour c = case ctFlavour c of
      Given   -> "G"
      Wanted  -> "W"
      Derived -> "D"
    formatCtType :: Ct -> String
    formatCtType c = pprToStr $ ctPred c

-- -----------------------------------------------------------------------------
-- Debug Functions
-- -----------------------------------------------------------------------------

-- | Print the result of calling 'show' on the given object.
printTrace :: (Show a) => a -> a
printTrace x = trace (show x) x

-- | Print the result of the 'Outputable' instance of the given object.
printObjTrace :: (Outputable o) => o -> o
printObjTrace o = trace (pprToStr o) o

-- -----------------------------------------------------------------------------
-- Debugging and printing from within TcPluginM
-- -----------------------------------------------------------------------------

-- | Internal function for printing from within the monad.
internalPrint :: String -> TcPluginM ()
internalPrint = tcPluginIO . putStr

-- | Print a message using the plugin formatting.
printMsg :: String -> TcPluginM ()
printMsg = internalPrint . smDebugMsg

-- | Print an error message using the plugin formatting.
printErr :: String -> TcPluginM ()
printErr = internalPrint . smErrMsg

-- | Print a warning message using the plugin formatting.
printWarn :: String -> TcPluginM ()
printWarn = internalPrint . smWarnMsg

-- | Print an object using the plugin formatting.
printObj :: Outputable o => o -> TcPluginM ()
printObj = internalPrint . smObjMsg . pprToStr

-- | Throw a type checker error with the given message.
pluginFailSDoc :: SDoc -> TcPluginM a
pluginFailSDoc msg = do
  printMsg $ sDocToStr msg
  unsafeTcPluginTcM $ failWithM (sDocToStr msg)

-- | If the given condition is false this will fail the compiler with the given error message.
pluginAssert :: Bool -> SDoc -> TcPluginM ()
pluginAssert True _ = return ()
pluginAssert False msg = pluginFailSDoc msg




