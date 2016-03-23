{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPMTIR                                               *
*       Purpose:        Simple pretty printer for MTIR                       *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2014                  *
*                                                                            *
******************************************************************************
-}

-- | Simple pretty printer for MTIR.

module PPMTIR (
    ppMTIR      -- MTIR -> String
) where

import Control.Supermonad.Prelude

-- HMTC module imports
import Name (Name)
import SrcPos
import Type (Type, retType)
import Symbol (TermSym, itmsType)
import PPUtilities
import MTIR


------------------------------------------------------------------------------
-- Pretty printing of MTIR
------------------------------------------------------------------------------

-- | Converts MTIR to a nicely laid-out textual representation for
-- display purposes.

ppMTIR :: MTIR -> String
ppMTIR mtir = ppCommand 0 (mtirCmd mtir) ""


------------------------------------------------------------------------------
-- Pretty printing of commands
------------------------------------------------------------------------------

ppCommand :: Int -> Command -> ShowS
ppCommand n (CmdAssign {caVar = v, caVal = e, cmdSrcPos = sp}) =
    indent n . showString "CmdAssign" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) v
    . ppExpression (n+1) e
ppCommand n (CmdCall {ccProc = p, ccArgs = es, cmdSrcPos = sp}) =
    indent n . showString "CmdCall" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) p
    . ppSeq (n+1) ppExpression es
ppCommand n (CmdSeq {csCmds = cs, cmdSrcPos = sp}) =
    indent n . showString "CmdSeq" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppCommand cs
ppCommand n (CmdIf {ciCondThens = ecs, ciMbElse = mc, cmdSrcPos = sp}) =
    indent n . showString "CmdIf" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) (\n (e,c) -> ppExpression n e . ppCommand n c) ecs
    . ppOpt (n+1) ppCommand mc
ppCommand n (CmdWhile {cwCond = e, cwBody = c, cmdSrcPos = sp}) =
    indent n . showString "CmdWhile" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e
    . ppCommand (n+1) c
ppCommand n (CmdRepeat {crBody = c, crCond = e, cmdSrcPos = sp}) =
    indent n . showString "CmdRepeat" . spc . ppSrcPos sp . nl
    . ppCommand (n+1) c
    . ppExpression (n+1) e
ppCommand n (CmdLet {clDecls = ds, clBody = c, cmdSrcPos = sp}) =
    indent n . showString "CmdLet" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppDeclaration ds
    . ppCommand (n+1) c


------------------------------------------------------------------------------
-- Pretty printing of expressions
------------------------------------------------------------------------------

ppExpression :: Int -> Expression -> ShowS
ppExpression n (ExpLitBool {elbVal = v, expType = t}) = 
    indent n . showString "ExpLitBool". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpLitInt {eliVal = v, expType = t}) = 
    indent n . showString "ExpLitInt". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpLitChr {elcVal = v, expType = t}) = 
    indent n . showString "ExpLitChr". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpExtRef {eerVal = l, expType = t}) = 
    indent n . showString "ExpExtRef". spc . shows l
    . showString " : " . shows t . nl
ppExpression n (ExpVar {evVar = v}) =
    indent n . showString "ExpVar" . spc . shows v . nl
ppExpression n (ExpDeref {edArg = e, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpDeref" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpApp {eaFun = f, eaArgs = es, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpApp" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) f
    . ppSeq (n+1) ppExpression es
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpCond {ecCond = e1, ecTrue = e2, ecFalse = e3,
                         expType = t, expSrcPos = sp}) = 
    indent n . showString "ExpCond" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e1
    . ppExpression (n+1) e2
    . ppExpression (n+1) e3
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpAry {eaElts = es, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpAry" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppExpression es
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpIx {eiAry = a, eiIx = i, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpIx" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) a
    . ppExpression (n+1) i
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpRcd {erFldDefs = fds, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpRcd" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) (\n (f,e) -> indent n . ppName f . nl . ppExpression n e) fds
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpPrj {epRcd = r, epFld = f, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpPrj" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) r
    . indent (n+1) . ppName f . nl
    . indent n . showString ": " . shows t . nl


------------------------------------------------------------------------------
-- Pretty printing of declarations
------------------------------------------------------------------------------

ppDeclaration :: Int -> Declaration -> ShowS
ppDeclaration n (DeclConst {dcConst = c, dcVal = e}) = 
    indent n . showString "DeclConst" . spc . ppSrcPos (srcPos c) . nl
    . indent (n+1) . shows c . nl
    . ppExpression (n+1) e
ppDeclaration n (DeclVar {dvVar = v, dvMbVal = me}) = 
    indent n . showString "DeclVar" . spc . ppSrcPos (srcPos v) . nl
    . indent (n+1) . shows v . nl
    . maybe id (ppExpression (n+1)) me
ppDeclaration n (DeclFun {dfFun = f, dfArgs = as, dfBody = e}) = 
    indent n . showString "DeclFun" . spc . ppSrcPos (srcPos f) . nl
    . indent (n+1) . shows f . nl
    . ppSeq (n+2) (\n a -> indent n . shows a . nl) as
    . indent (n+1) . showString ": " . shows (retType (itmsType f)) . nl 
    . ppExpression (n+1) e
ppDeclaration n (DeclProc {dpProc = p, dpArgs = as, dpBody = c}) = 
    indent n . showString "DeclProc" . spc . ppSrcPos (srcPos p) . nl
    . indent (n+1) . shows p . nl
    . ppSeq (n+2) (\n a -> indent n . shows a . nl) as
    . ppCommand (n+1) c
