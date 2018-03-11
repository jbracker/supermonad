{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPAST                                                *
*       Purpose:        Simple pretty printer for AST                        *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2014                  *
*                                                                            *
******************************************************************************
-}

-- | Simple pretty printer for AST.

module PPAST (
    ppAST       -- AST -> String
) where

-- HMTC module imports
import Name (Name)
import SrcPos (SrcPos)
import PPUtilities
import AST


------------------------------------------------------------------------------
-- Pretty printing of AST
------------------------------------------------------------------------------

-- | Converts AST to a nicely laid-out textual representation for
-- display purposes.

ppAST :: AST -> String
ppAST ast = ppCommand 0 (astCmd ast) ""


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
ppExpression n (ExpLitInt {eliVal = v}) = 
    indent n . showString "ExpLitInt". spc . shows v . nl
ppExpression n (ExpLitChr {elcVal = v}) = 
    indent n . showString "ExpLitChr". spc . shows v . nl
ppExpression n (ExpVar {evVar = v}) =
    indent n . showString "ExpVar" . spc . ppName v . nl
ppExpression n (ExpApp {eaFun = f, eaArgs = es, expSrcPos = sp}) =
    indent n . showString "ExpApp" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) f
    . ppSeq (n+1) ppExpression es
ppExpression n (ExpAry {eaElts = es, expSrcPos = sp}) =
    indent n . showString "ExpAry" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppExpression es
ppExpression n (ExpIx {eiAry = a, eiIx = i, expSrcPos = sp}) =
    indent n . showString "ExpIx" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) a
    . ppExpression (n+1) i
ppExpression n (ExpRcd {erFldDefs = fds, expSrcPos = sp}) =
    indent n . showString "ExpRcd" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) (\n (f,e) -> indent n . ppName f . nl . ppExpression n e) fds
ppExpression n (ExpPrj {epRcd = r, epFld = f, expSrcPos = sp}) =
    indent n . showString "ExpPrj" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) r
    . indent (n+1) . ppName f . nl
ppExpression n (ExpCond {ecCond = c, ecTrue = t, ecFalse = f, expSrcPos = sp})=
    indent n . showString "ExpCond" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) c
    . ppExpression (n+1) t
    . ppExpression (n+1) f


------------------------------------------------------------------------------
-- Pretty printing of declarations
------------------------------------------------------------------------------

ppDeclaration :: Int -> Declaration -> ShowS
ppDeclaration n (DeclConst {dcConst = c, dcType = t, dcVal = e,
                            declSrcPos = sp}) = 
    indent n . showString "DeclConst" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName c . nl
    . ppTypeDenoter (n+1) t
    . ppExpression (n+1) e
ppDeclaration n (DeclVar {dvVar = v, dvType = t, dvMbVal = me,
                          declSrcPos = sp}) = 
    indent n . showString "DeclVar" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName v . nl
    . ppTypeDenoter (n+1) t
    . maybe id (ppExpression (n+1)) me
ppDeclaration n (DeclFun {dfFun = f, dfArgDecls = as, dfType = t, dfBody = e,
                          declSrcPos = sp}) = 
    indent n . showString "DeclFun" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName f . nl
    . ppSeq (n+1) ppArgDecl as 
    . ppTypeDenoter (n+1) t
    . ppExpression (n+1) e
ppDeclaration n (DeclProc {dpProc = p, dpArgDecls = as, dpBody = c,
                           declSrcPos = sp}) = 
    indent n . showString "DeclProc" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName p . nl
    . ppSeq (n+1) ppArgDecl as 
    . ppCommand (n+1) c


------------------------------------------------------------------------------
-- Pretty printing of argument declarations
------------------------------------------------------------------------------

ppArgDecl :: Int -> ArgDecl -> ShowS
ppArgDecl n (ArgDecl {adArg = a, adArgMode = am, adType = t, adSrcPos = sp}) =
    indent n . showString "ArgDecl" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName a . nl
    . indent (n+1) . showString (show am) . nl
    . ppTypeDenoter (n+1) t


------------------------------------------------------------------------------
-- Pretty printing of type denoters
------------------------------------------------------------------------------

ppTypeDenoter :: Int -> TypeDenoter -> ShowS
ppTypeDenoter n (TDBaseType {tdbtName = tn}) = 
    indent n . showString "TDBaseType" . spc . ppName tn . nl
ppTypeDenoter n (TDArray {tdaEltType = et, tdaSize = s}) = 
    indent n . showString "TDArray" . nl
    . ppTypeDenoter (n+1) et
    . indent (n+1) . showString (show s) . nl
ppTypeDenoter n (TDRecord {tdrFldTypes = fts}) = 
    indent n . showString "TDRecord" . nl
    . ppSeq (n+1) (\n (f,t) -> indent n . ppName f . nl . ppTypeDenoter n t)
                  fts
