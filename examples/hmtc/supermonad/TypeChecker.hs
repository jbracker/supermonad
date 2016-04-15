{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         TypeChcker                                           *
*       Purpose:        MiniTriangle Type Checker                            *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | MiniTriangle Type Checker. 

-- Substantially re-written autumn 2013

module TypeChecker (
    typeCheck,          -- :: A.AST -> D MTIR
    testTypeChecker     -- :: String -> [(Name,Type)] -> IO ()
) where

import Control.Supermonad.Prelude


-- Standard library imports
import Data.List ((\\), nub, sort, intersperse)
import Data.Maybe (fromJust)
import Control.Monad (mapAndUnzipM, unless)
import Control.Monad.Fix (mfix)

import Debug.Trace (trace, traceShow) 

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import ScopeLevel
import Type
import Symbol
import Env
import MTStdEnv
import qualified AST as A
import MTIR
import PPMTIR
import Parser (parse)

-- | Type checks a complete MiniTriangle program in the standard environment 
-- and reports any errors. Hence a computation in the diagnostics monad 'D'.
-- Additionally, translates the program into the type-annotated, intermediate
-- representation MTIR, including eliminating implicit dereferencing at the
-- source level by inserting explicit dereferencing operations.

-- See the Coursework Description Part II for an explanation of the concepts
-- and principles behind this type checker, along with the typing rules that
-- define the MiniTriangle type system which this type checker implements.
-- In particular, comments like "T-IF", "env(x) = s" and "env |- e : t"
-- refers to those typing rules, with the following naming conventions:
--
--      Type set typing rule                    ASCII Comments
--      --------------------                    --------------
--      Capital gamma                           "env"
--      turnstile                               "|-"
--      vector notation (e.g. "e" overlined)    plural "s" suffix, e.g. "es"

typeCheck :: A.AST -> D MTIR
typeCheck (A.AST {A.astCmd = c}) = do
    c' <- chkCmd mtStdEnv c
    return (MTIR {mtirCmd = c'}) 


------------------------------------------------------------------------------
-- Implementation of main typing rules
------------------------------------------------------------------------------

-- Check that command is well-formed in the given environment:
--
--     env |- c

chkCmd :: Env -> A.Command -> D Command
-- T-ASSIGN
chkCmd env (A.CmdAssign {A.caVar = x, A.caVal = e, A.cmdSrcPos = sp}) = do
    (s, x') <- infTpExp env x                   -- env |- x : s
    (t, x'')<- sinks_nonreftype s x'            -- sinks(s,t), not reftype(t)
    e'      <- chkTpExp env e t                 -- env |- e : t
    return (CmdAssign {caVar = x'', caVal = e', cmdSrcPos = sp})
-- T-CALL
chkCmd env (A.CmdCall {A.ccProc = p, A.ccArgs = es, A.cmdSrcPos = sp}) = do
    (ss, es')   <- mapAndUnzipM (infTpExp env) es
    (ts, t, p') <- infArrTpExp env p ss                 -- env |- p : ts->Void
    require (t == Void) sp (notProcMsg t)
    es''        <- zipWith3M sources ss ts es'          -- env |- es : ts
    return (CmdCall {ccProc = p', ccArgs = es'', cmdSrcPos = sp})
    where
        notProcMsg t = "Not a procedure; return type is " ++ show t
-- T-SEQ (generalized to sequence of any length)
chkCmd env (A.CmdSeq {A.csCmds = cs, A.cmdSrcPos = sp}) = do
    cs' <- mapM (chkCmd env) cs
    return (CmdSeq {csCmds = cs', cmdSrcPos = sp})
-- T-IF
chkCmd env (A.CmdIf {A.ciCondThens = ecs, A.ciMbElse = mbce,
                     A.cmdSrcPos=sp}) = do
    ecs'  <- mapM (tcCondThen env) ecs
                                                        -- env |- cs
    mbce' <- case mbce of
                 Nothing -> return Nothing
                 Just ce -> do
                     ce' <- chkCmd env ce               -- env |- ce
                     return (Just ce')
    return (CmdIf {ciCondThens = ecs', ciMbElse = mbce', cmdSrcPos = sp})
    where
        tcCondThen :: Env -> (A.Expression, A.Command)
                      -> D (Expression, Command)
        tcCondThen env (e, c) = do
            e' <- chkTpExp env e Boolean
            c' <- chkCmd env c
            return (e', c')
-- T-WHILE
chkCmd env (A.CmdWhile {A.cwCond = e, A.cwBody = c, A.cmdSrcPos = sp}) = do
    e' <- chkTpExp env e Boolean                        -- env |- e : Boolean
    c' <- chkCmd env c                                  -- env |- c
    return (CmdWhile {cwCond = e', cwBody = c', cmdSrcPos = sp})
-- T-REPEAT
chkCmd env (A.CmdRepeat {A.crBody = c, A.crCond = e, A.cmdSrcPos = sp}) = do
    c' <- chkCmd env c                                  -- env |- c
    e' <- chkTpExp env e Boolean                        -- env |- e : Boolean
    return (CmdRepeat {crBody = c', crCond = e', cmdSrcPos = sp})
-- T-LET
chkCmd env (A.CmdLet {A.clDecls = ds, A.clBody = c, A.cmdSrcPos = sp}) = do
    (ds', env') <- mfix $ \ ~(_, env') ->               -- env;env'|- ds | env'
                       chkDeclarations (openMinScope env) env' ds 
    c'          <- chkCmd env' c                        -- env' |- c
    return (CmdLet {clDecls = ds', clBody = c', cmdSrcPos = sp})


-- Check that declarations/definitions are well-typed in given environment
-- and environmant for function/procedure bodies and compute extended
-- environment:
--
--     env; envB |- ds | env'
--
-- [For future reference: If user defined types were allowed, envB should
-- perhaps be used in place of env for elaboarting types and computing
-- function/procedure types if it is desired to allow (mutually) recursive
-- type definitions.]

chkDeclarations :: Env -> Env -> [A.Declaration] -> D ([Declaration], Env)
-- T-DECLEMPTY
chkDeclarations env envB [] = return ([], env)
-- T-DECLCONST
chkDeclarations env envB 
                (A.DeclConst {A.dcConst = x, A.dcVal = e, A.dcType = t,
                              A.declSrcPos=sp}
                 : ds) = do
    t' <- chkDclType env t
    e' <- chkTpExp env e t'                         -- env |- e : t
    case enterIntTermSym x (Src t') sp env of       -- env' = env, x : Src t
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkDeclarations env envB ds
        Right (env', x') -> do                      
            wellinit (itmsLvl x') e'
            (ds', env'') <- chkDeclarations env'    -- env'; envB |- ds | env''
                                            envB
                                            ds
            return (DeclConst {dcConst = x', dcVal = e'} : ds', env'')
-- T-DECLVAR
chkDeclarations env envB 
                (A.DeclVar {A.dvVar = x, A.dvType = t, A.dvMbVal = Nothing,
                            A.declSrcPos=sp}
                 : ds) = do
    t' <- chkDclType env t
    case enterIntTermSym x (Ref t') sp env of       -- env' = env, x : Ref t
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkDeclarations env envB ds
        Right (env', x') -> do                      
            (ds', env'') <- chkDeclarations env'    -- env'; envB |- ds | env''
                                            envB
                                            ds
            return (DeclVar {dvVar = x', dvMbVal = Nothing} : ds', env'')
-- T-DECLINITVAR
chkDeclarations env envB
                (A.DeclVar {A.dvVar = x, A.dvType = t, A.dvMbVal = Just e,
                            A.declSrcPos=sp}
                 : ds) = do
    t' <- chkDclType env t
    e' <- chkTpExp env e t'                         -- env |- e : t
    case enterIntTermSym x (Ref t') sp env of       -- env' = env, x : Ref t
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkDeclarations env envB ds
        Right (env', x') -> do
            wellinit (itmsLvl x') e'
            (ds', env'') <- chkDeclarations env'    -- env'; envB |- ds | env''
                                            envB
                                            ds
            return (DeclVar {dvVar = x', dvMbVal = Just e'} : ds', env'')
-- T-DECLFUN
chkDeclarations env envB
                (A.DeclFun {A.dfFun = f, A.dfOvrld = o,
                            A.dfArgDecls = as, A.dfType = t, A.dfBody = e,
                            A.declSrcPos = sp}
                 : ds) = do
    ~(as', envB') <- chkArgDecls (openMajScope envB) as -- envB |- as | envB'
    tf            <- funType env as t
    e'            <- chkTpExp envB' e (retType tf)      -- envB' |- e : t
    case enterSym f tf sp env of                        -- env' = env, f: tf
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkDeclarations env envB ds
        Right (env', f') -> do                      
            (ds', env'') <- chkDeclarations env'    -- env'; envB |- ds | env''
                                            envB
                                            ds
            return (DeclFun {dfFun = f', dfArgs = as', dfBody = e'} : ds',
                    env'')
    where
        enterSym = if o then enterOvrldIntTermSym else enterIntTermSym
-- T-DECLPROC
chkDeclarations env envB
                (A.DeclProc {A.dpProc = p, A.dpOvrld = o,
                             A.dpArgDecls = as, A.dpBody = c, 
                             A.declSrcPos = sp}
                 : ds) = do
    ~(as', envB') <- chkArgDecls (openMajScope envB) as -- envB |- as | envB'
    c'            <- chkCmd envB' c                 -- envB' |- c
    tp            <- procType env as
    case enterSym p tp sp env of                    -- env' = env, f: tf
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkDeclarations env envB ds
        Right (env', p') -> do                      
            (ds', env'') <- chkDeclarations env'    -- env'; envB |- ds | env''
                                            envB
                                            ds
            return (DeclProc {dpProc = p', dpArgs = as', dpBody = c'} : ds',
                    env'')
    where
        enterSym = if o then enterOvrldIntTermSym else enterIntTermSym


-- Check that function/procedure argument declarations are well-typed in
-- given environment and compute extended environment:
--
--     env |- as | env'

chkArgDecls :: Env -> [A.ArgDecl] -> D ([IntTermSym], Env)
-- T-DECLARGEMPTY
chkArgDecls env [] = return ([], env)
-- T-DECLARG, T-DECLINARG, T-DECLOUTARG, T-DECLVARARG
chkArgDecls env  
            (A.ArgDecl {A.adArg = x, A.adArgMode = am, A.adType = td,
             A.adSrcPos=sp}
            : as) = do
    t <- chkDclType env td
    case enterIntTermSym x (Src (amType am t)) sp env of -- env' = env, x: ...
        Left old -> do
            emitErrD sp (redeclaredMsg old)
            chkArgDecls env as
        Right (env', x') -> do                          
            (as', env'') <- chkArgDecls env' as         -- env' |- as | env''
            return (x' : as', env'')


redeclaredMsg :: IntTermSym -> String
redeclaredMsg itms =
    "Identifier \""
    ++ itmsName itms
    ++ "\" redeclared; already declared at "
    ++ show (itmsSrcPos itms)


procType :: Env -> [A.ArgDecl] -> D Type
procType env as = do
    ts <- chkArgTypes env as
    return (Arr ts Void)


funType :: Env -> [A.ArgDecl] -> A.TypeDenoter -> D Type
funType env as td = do
    ts <- chkArgTypes env as
    t  <- chkDclType env td
    return (Arr ts t)


chkArgTypes :: Env -> [A.ArgDecl] -> D [Type]
chkArgTypes env []                                                 = return []
chkArgTypes env (A.ArgDecl {A.adArgMode = am, A.adType = td} : as) = do
    t  <- chkDclType env td
    ts <- chkArgTypes env as
    return (amType am t : ts)


-- Checks that a given type is defined and translate into internal type
-- representation.
chkDclType :: Env -> A.TypeDenoter -> D Type
chkDclType env (A.TDBaseType {A.tdbtName = t, A.tdSrcPos = sp}) =
    case lookupTypeSym t env of
        Nothing -> do
            emitErrD sp ("Undefined type \"" ++ t ++ "\"")
            return SomeType
        Just tps ->
            return (tpsType tps)
chkDclType env (A.TDArray {A.tdaEltType = t, A.tdaSize = s,
                A.tdSrcPos = sp}) = do
    t' <- chkDclType env t
    s' <- toMTInt s sp
    return (Ary t' s')
chkDclType env (A.TDRecord {A.tdrFldTypes = fts}) = do
    -- Note: Ensures record fields are sorted (invariant of Rcd)
    let (xs,ts) = unzip fts
    ts' <- mapM (chkDclType env) ts
    return (Rcd (sortRcdFlds (zip xs ts')))


-- Type representation corresponding to given argument mode

amType :: A.ArgMode -> (Type -> Type)
amType A.ByValue  = id          -- Call-by-value
amType A.ByRefIn  = Src         -- Call-by-ref input
amType A.ByRefOut = Snk         -- Call-by-ref output
amType A.ByRefVar = Ref         -- Call-by-ref variable


-- Check that expression has type t in given environment:
--
--     env |- e : t
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is known. Knowing the target type
-- makes it easy to use the rule T-SOURCES.

chkTpExp :: Env -> A.Expression -> Type -> D Expression
-- T-SOURCES
chkTpExp env e t = do
    (s, e') <- infTpExp env e                   -- env |- e : s, sources(s,t)
    sources s t e'


-- Check that expression is well-typed in the given environment and
-- infer its type assuming no (top-level) implicit dereferencing:
--
--     env |- e : t
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is not known and leaving the option
-- for further dereferencing open.

infTpExp :: Env -> A.Expression -> D (Type, Expression)
-- T-LITCHR
infTpExp env e@(A.ExpLitChr {A.elcVal = c, A.expSrcPos = sp}) = do
     c' <- toMTChr c sp
     return (Character,                         -- env |- c : Character
             ExpLitChr {elcVal = c', expType = Character, expSrcPos = sp})
-- T-LITINT
infTpExp env e@(A.ExpLitInt {A.eliVal = n, A.expSrcPos = sp}) = do
    n' <- toMTInt n sp
    return (Integer,                            -- env |- n : Integer
            ExpLitInt {eliVal = n', expType = Integer, expSrcPos = sp})
-- T-VAR
infTpExp env (A.ExpVar {A.evVar = x, A.expSrcPos = sp}) = do
    tms <- case lookupTermSym x env of          -- env(x) = t, sources(t,t)
           Nothing -> do
               emitErrD sp ("Variable \"" ++ x ++ "\" undefined")
               return (dummyTmS x)
           Just tms -> return tms
    return (tmsType tms, tmsToExp tms sp)
-- T-APP
infTpExp env (A.ExpApp {A.eaFun = f, A.eaArgs = es, A.expSrcPos = sp}) = do
    (ss, es')   <- mapAndUnzipM (infTpExp env) es
    (ts, t, f') <- infArrTpExp env f ss                 -- env |- f : ts -> t
    es''        <- zipWith3M sources ss ts es'          -- env |- es : ts
    return (t, ExpApp {eaFun = f', eaArgs = es'', expType = t, expSrcPos = sp})
infTpExp env (A.ExpCond {A.ecCond = e1, A.ecTrue = e2,
                             A.ecFalse = e3, A.expSrcPos = sp}) = do
    e1'       <- chkTpExp env e1 Boolean                -- Env |- e1 : Boolean
    (t, e2')  <- infNonRefTpExp env e2                  -- Env |- e2 : t
    (t', e3') <- infNonRefTpExp env e3                  -- Env |- e3 : t'
    require (t == t') sp ("Conditional branches have to have the same \
                          \types; "
                          ++ "got " ++ show t ++ " and " ++ show t')
    return (t,
            ExpCond {ecCond = e1', ecTrue = e2', ecFalse = e3',
                     expType = t, expSrcPos = sp})
-- T-ARY, empty case handled specially
infTpExp env (A.ExpAry {A.eaElts = [], A.expSrcPos = sp}) = do
    let t = Ary SomeType 0
    return (t, ExpAry {eaElts = [], expType = t, expSrcPos = sp})
infTpExp env (A.ExpAry {A.eaElts = ees@(e:es), A.expSrcPos = sp}) = do
    (t, e') <- infNonRefTpExp env e             -- env |- e : t, not reftype(t)
    es'     <- mapM (\e -> chkTpExp env e t) es
    let ta = Ary t (fromIntegral (length ees))
    return (ta, ExpAry {eaElts = e':es', expType = ta, expSrcPos = sp})
-- T-IX
infTpExp env (A.ExpIx {A.eiAry = a, A.eiIx = i, A.expSrcPos = sp}) = do
    (rat, a') <- infRefAryTpExp env a           -- env |- a : R(T[n])
    i'        <- chkTpExp env i Integer         -- end |- i : Integer
    let rt = mapRfcdType eltType rat
    return (rt, ExpIx {eiAry = a', eiIx = i', expType = rt, expSrcPos = sp})
-- T-RCD
infTpExp env (A.ExpRcd {A.erFldDefs = fds, A.expSrcPos = sp}) = do
    -- Note: Ensures record fields are sorted (invariant of ExpRcd and Rcd)
    let (xs, es) = unzip (sortRcdFlds fds)
    tes' <- mapM (infNonRefTpExp env) es
    require (allDistinct xs) sp (repeatedMsg xs)
    let (ts, es') = unzip tes'
    let fds'      = zip xs es'
    let tr        = Rcd (zip xs ts)
    return (tr, ExpRcd {erFldDefs = fds', expType = tr, expSrcPos = sp})
    where
        allDistinct xs = xs == nub xs
        repeatedMsg xs = "Repeated record field name(s): \""
                         ++ concat (intersperse "\", \"" (nub (xs \\ nub xs)))
                         ++ "\"" 
-- T-PRJ
infTpExp env (A.ExpPrj {A.epRcd = e, A.epFld = f, A.expSrcPos = sp}) = do
    (rrt, e') <- infRefRcdTpExp env e           -- env |- e : R({xs:ts})
    require (fldExists f (rfcdType rrt)) sp (notAFieldMsg f (rfcdType rrt))
    let rt = mapRfcdType (fldType f) rrt
    return (rt, ExpPrj {epRcd = e', epFld = f, expType = rt, expSrcPos = sp})
    where
        notAFieldMsg f rt = "The type \"" ++ show rt
                            ++ "\" does not contain any field \"" ++ f ++ "\"" 


-- Check that expression is well-typed in the given environment and
-- infer its type assuming it should be an non-reference type:
--
--     env |- e : t, not reftype(t)
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is known to be a non-reference type.

infNonRefTpExp :: Env -> A.Expression -> D (Type, Expression)
infNonRefTpExp env e = do
    (t, e') <- infTpExp env e
    sources_pred (not . refType) "non-reference type" t e' 


-- Check that expression is well-typed in the given environment and
-- infer its type assuming it should be an arrow type and given the
-- inferred but not dereferenced types of the actual arguments:
--
--     env |- e : ts -> t
--
-- The caller is responsible for checking that the type of each actual
-- argument can source the type of the corresponding formal argument and
-- dereference the argument expression as necessary (use "sources").
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is known to be an arrow type.
--
-- The number of actual arguments is used to check the arity of expression
-- type, reporting any mismatch and guaranteeing that the arity of the
-- returned type agrees.
--
-- The number and types of the actual arguments are used for resolving
-- overloading in the case of *manifest* calls (the name of the procedure
-- or function given directly as opposed to a procedure or function being
-- computed). 

infArrTpExp :: Env -> A.Expression -> [Type] -> D ([Type], Type, Expression)
infArrTpExp env e@(A.ExpVar {A.evVar = x}) ss
    | null tmss = do
        emitErrD sp ("No procedure or function called \"" ++ x ++ "\"")
        return (ensureArity a [], SomeType, tmsToExp (dummyTmS x) sp)
    | arrType (tmsType (head tmss)) =
    -- Manifest procedure or function call. Overloading resolution only carried
    -- out for manifest calls. A call is considered manifest if at least the
    -- first of any overloadings of the applied symbol is a manifest procedure
    -- or function (arrow type).
        case tmssCA of
            [] -> do
                emitErrD sp ("No instance of \"" ++ x
                             ++ "\" has expected arity " ++ show a)
                return (ensureArity a [], SomeType,
                        tmsToExp (dummyTmS x) sp)
            [tms] ->
                case tmsType tms of
                    Arr ts t -> return (ts, t, tmsToExp tms sp)
                    _        -> tcErr "infArrTpExp" "Expected arrow type"
            _ ->
                case tmssCS of
                    [] -> do
                        emitErrD sp ("No instance of \"" ++ x
                                     ++ "\" has a signature that agrees with \
                                        \the types of the actual arguments: "
                                     ++ concat(intersperse ", " (map show ss)))
                        return (ensureArity a [], SomeType,
                                tmsToExp (dummyTmS x) sp)
                    [tms] ->
                        case tmsType tms of
                            Arr ts t -> return (ts, t, tmsToExp tms sp)
                            _        -> tcErr "infArrTpExp"
                                              "Expected arrow type"
                    (tms : _) -> do
                        emitWngD sp ("Ambiguous overloading of \"" ++ x
                                     ++ "\": more than one instance have \
                                        \a signature that agrees with the \
                                        \types of the actual arguments")
                        case tmsType tms of
                            Arr ts t -> return (ts, t, tmsToExp tms sp)
                            _        -> tcErr "infArrTpExp"
                                              "Expected arrow type"
    where
        sp     = srcPos e
        a      = length ss                      -- Expected arity
        tmss   = lookupOvrldTermSym x env
        tmssCA = [ tms                          -- Tm syms with Correct Arity
                 | tms <- tmss,
                   let t = tmsType tms,
                   arrType t && arity t == a
                 ] 
        tmssCS = [ tms                          -- Tm syms with Compatible Sig.
                 | tms <- tmssCA,
                   and (zipWith sourcesp ss (argTypes (tmsType tms)))
                 ]
infArrTpExp env e ss = do
    -- This is the case of a computation yielding (a reference to) a
    -- procedure or function. No overloading resolution in this case.
    (s, e')   <- infNonRefTpExp env e
    case s of
        Arr ts t -> do
            require (length ts == a) sp
                    ("Bad arity: expected " ++ show (length ts)
                     ++ " arguments, got " ++ show a) 
            return (ensureArity a ts, t, e')
        SomeType -> do
            return (ensureArity a [], SomeType, e')
        _ -> do
            emitErrD sp "Not a function or procedure"
            return (ensureArity a [], SomeType, e')
    where
        sp = srcPos e
        a  = length ss                          -- Expected arity


ensureArity :: Int -> [Type] -> [Type]
ensureArity a ts = take a (ts ++ repeat SomeType)


-- Check that expression is well-typed in the given environment and
-- infer its type assuming it should be a reference to an array:
--
--     env |- e : R (t[n]), R in {Src, Snk, Ref}
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is known to be a reference to an array.

infRefAryTpExp :: Env -> A.Expression -> D (Type, Expression)
infRefAryTpExp env e = do
    (t, e') <- infTpExp env e
    sources_pred refAry "reference to array" t e' 
    where
        refAry (Src (Ary _ _)) = True
        refAry (Snk (Ary _ _)) = True
        refAry (Ref (Ary _ _)) = True
        refAry _               = False


-- Check that expression is well-typed in the given environment and
-- infer its type assuming it should be a reference to a record:
--
--     env |- e : R ({a : t, ...}), R in {Src, Snk, Ref}
--
-- This is an algorithmic version of the typing relation for expressions
-- to be used when the desired type is known to be a reference to a record.

infRefRcdTpExp :: Env -> A.Expression -> D (Type, Expression)
infRefRcdTpExp env e = do
    (t, e') <- infTpExp env e
    sources_pred refRcd "reference to record" t e' 
    where
        refRcd (Src (Rcd _)) = True
        refRcd (Snk (Rcd _)) = True
        refRcd (Ref (Rcd _)) = True
        refRcd _             = False


-- Convert Integer to MTInt (MiniTriangle integer), ensuring it is
-- representable as such.

toMTInt :: Integer -> SrcPos -> D MTInt
toMTInt n sp =
   if isMTInt n then
       return (fromInteger n)
   else do
       emitErrD sp ("Integer literal " ++ show n ++ " outside the range of "
                     ++ "representable MiniTriangle integers")
       return 0


-- Convert Char to MTChr (MiniTriangle character), ensuring it is
-- representable as such.

toMTChr :: Char -> SrcPos -> D MTChr
toMTChr c sp =
   if isMTChr c then
       return c         -- MTChr is currently just a type synonym
   else do
       emitErrD sp ("Character literal " ++ show c ++ " outside the range of "
                     ++ "representable MiniTriangle characters")
       return '?'


-- Converts an (internal or external) term symbol into an MTIR expression:
-- a variable for internal symbols, or constant/external reference for
-- an external symbol.

tmsToExp :: TermSym -> SrcPos -> Expression
tmsToExp (Left (ExtTermSym {etmsVal = v, etmsType = t})) sp =
    case v of
        ESVBool b ->
            ExpLitBool {elbVal = b, expType = t, expSrcPos = sp}
        ESVInt n ->
            ExpLitInt {eliVal = n, expType = t, expSrcPos = sp}
        ESVChar c ->
            ExpLitChr {elcVal = c, expType = t, expSrcPos = sp}
        ESVLbl l ->
            ExpExtRef {eerVal = l, expType = t, expSrcPos = sp}
tmsToExp (Right itms@(IntTermSym  {itmsType = t})) sp =
    ExpVar {
        evVar     = itms,
        expType   = t,
        expSrcPos = sp
    }


------------------------------------------------------------------------------
-- Implementation of auxiliary predicates
------------------------------------------------------------------------------


-- Check if the value of an expression of the given type can source a value
-- of the other given type. This is a version of the predicate "sources"
-- from the type system specification turned into a function assuming both
-- types are known.
-- Additionally "coerces" the type of the expression by embedding it in
-- the appropriate number of dereferencing operations.

sources :: Type -> Type -> Expression -> D Expression
sources s       t       e | s <: t = return e
sources (Ref s) t       e          = sources s t (deref e)
sources (Src s) t       e          = sources s t (deref e)
sources s       t       e          = do
    emitErrD (srcPos e)
             ("Expected type \"" ++ show t ++ "\", got \"" ++ show s ++ "\"")
    return e


-- Predicate version of the above.

sourcesp :: Type -> Type -> Bool
sourcesp s       t | s <: t = True
sourcesp (Ref s) t          = sourcesp s t
sourcesp (Src s) t          = sourcesp s t
sourcesp _       _          = False


{-
-- Alternative definition without explicit use of subtyping for reference. 
-- Somewhat less flexible and a bit more verbose, but adequate for
-- MiniTriangle as it stands, and avoiding subtyping is a simplification.
sources :: Type -> Type -> Expression -> D Expression
sources s       t       e | s == t = return e
sources (Ref s) (Snk t) e | s == t = return e
sources (Ref s) (Src t) e | s == t = return e
sources (Ref s) t       e          = sources s t (deref e)
sources (Src s) t       e          = sources s t (deref e)
sources s       t       e          = do
    emitErrD (srcPos e)
             ("Expected type \"" ++ show t ++ "\", got \"" ++ show s ++ "\"")
    return e
-}


-- Check if the value of an expression of the given type can source a type
-- satisfying an additinal predicate p. That is, an implementation of the
-- combination:
--
--     sources(s,t), p(t)
--
-- assuming type "s" is given and "t" is to be computed.
-- Additionally "coerces" the type of the expression by embedding it in
-- the appropriate number of dereferencing operations.

sources_pred :: (Type -> Bool) -> String -> Type -> Expression
                -> D (Type, Expression)
sources_pred p et t       e | p t = return (t, e)
sources_pred p et (Ref t) e       = sources_pred p et t (deref e)
sources_pred p et (Src t) e       = sources_pred p et t (deref e)
sources_pred p et t       e       = do
    emitErrD (srcPos e) 
             ("Expected " ++ et ++ ", got \"" ++ show t ++ "\"")
    return (SomeType, e)


-- Check if the value of an expression of the given type can sink a non-
-- reference type. That is, an implementation of the combination:
--
--     sinks(s,t), not reftype(t)
--
-- assuming type "s" is given and "t" is to be computed.
-- Additionally "coerces" the type of the expression by embedding it in
-- the appropriate number of dereferencing operations.

sinks_nonreftype :: Type -> Expression -> D (Type, Expression)
sinks_nonreftype (Snk t) e
    | not (refType t) = return (t, e)
    | otherwise       = do
        emitErrD (srcPos e)
                 "Cannot assign value of non-reference type to this variable"
        return (SomeType, e)
sinks_nonreftype (Src t) e = 
    sinks_nonreftype t (deref e)
sinks_nonreftype (Ref t) e
    | not (refType t) = return (t, e)
    | otherwise       = sinks_nonreftype t (deref e)
sinks_nonreftype SomeType e =
    return (SomeType, e)
sinks_nonreftype _ e = do
    emitErrD (srcPos e) "Does not denote an assignable variable"
    return (SomeType, e)


-- Embeds an expression in a dereferencing operation.

deref :: Expression -> Expression
deref e =
    ExpDeref {
        edArg     = e,
        expType   = rfcdType (expType e),
        expSrcPos = srcPos e
    }


-- Check that an initialiser (expression defining a constant or initialising
-- a variable) is well-initialised; i.e. does not call a function defined
-- at the present scope level as this means it could refer to constants
-- or variables that have not yet been initialised or even allocated.
-- This is defined on MTIR rather than AST, simplifying the definition
-- as the scope level of a variable is directly available, meaning that
-- there is no need to look up variables in the environment.
--
-- [For future reference:
-- Note that this restriction could be relaxed. For example, one could
-- for each function compute the set of constants/variables from the
-- same block to which it refers directly or indirectly, and then
-- flag a problem only if a function referring to constants/variables
-- currently not in scope are called. But, as functions may be mutually
-- recursive, this requires computing strongly connected components or
-- an iterative fixed-point computation.
--
-- Or one could implement initialization in dependency order, combined
-- with a static check that there are no cycles. Or one could reorder
-- declaration lists, moving procedures and functions to the end,
-- either after type checking and then possibly combined with adapted
-- code generation strategies, or before type checking, combined with
-- changed scope rules (bringing all functions and procedures into scope
-- at the start of a block) and possibly with adapted code generation
-- strategies.]

wellinit :: ScopeLvl -> Expression -> D ()
wellinit _ (ExpLitBool {})        = return ()
wellinit _ (ExpLitInt {})         = return ()
wellinit _ (ExpLitChr {})         = return ()
wellinit _ (ExpExtRef {})         = return ()
wellinit _ (ExpVar {})            = return ()
wellinit l (ExpDeref {edArg = e}) = wellinit l e
wellinit l (ExpApp {eaFun = f, eaArgs = es}) = do
    case f of
        ExpLitBool {} -> return ()      -- Type error, will have been caught
        ExpLitInt {}  -> return ()      -- Type error, will have been caught
        ExpLitChr {}  -> return ()      -- Type error, will have been caught
        ExpExtRef {}  -> return ()      -- Defined outside present scope
        ExpVar {evVar = IntTermSym {itmsLvl = l', itmsName = n},
                expSrcPos = sp}
            | l' == l ->
                emitErrD sp
                         ("Function \""
                          ++ n 
                          ++ "\" may not be called from initializers in the \
                             \same block as in which it is defined.")
            | otherwise -> return ()
        e ->
            emitErrD (srcPos e)
                     "Only known functions may be called in initializers."
    mapM_ (wellinit l) es
wellinit l (ExpCond {ecCond = e1, ecTrue = e2, ecFalse = e3}) =
    wellinit l e1 >> wellinit l e2 >> wellinit l e3
wellinit l (ExpAry {eaElts = es}) = mapM_ (wellinit l) es
wellinit l (ExpIx {eiAry = a, eiIx = i}) = wellinit l a >> wellinit l i
wellinit l (ExpRcd {erFldDefs = fds}) = mapM_ (wellinit l . snd) fds
wellinit l (ExpPrj {epRcd = e}) = wellinit l e


------------------------------------------------------------------------------
-- Error reporting utilities
------------------------------------------------------------------------------

-- Report an error unless the condition is true.

require :: Bool -> SrcPos -> String -> D ()
require p sp m = unless p (emitErrD sp m)


------------------------------------------------------------------------------
-- Monadic utilities
------------------------------------------------------------------------------

-- Generalisation of zipWithM

zipWith3M :: (Bind m m m, Return m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f (a:as) (b:bs) (c:cs) = do
    d  <- f a b c
    ds <- zipWith3M f as bs cs
    return (d:ds)
zipWith3M _ _ _ _ = return []


------------------------------------------------------------------------------
-- Test utilities
------------------------------------------------------------------------------

-- | Test utility. Attempts to parse and then type check the given string
-- input in the standard MT environment extended with any given bindings.
-- If successful, pretty-prints the resulting MTIR representation.

testTypeChecker :: String -> [(Name,Type)] -> IO ()
testTypeChecker s bs = do
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) (snd result)
    putStrLn ""
    case fst result of
        Just mtir -> do
                         putStrLn "MTIR:"
                         putStrLn (ppMTIR mtir)
        Nothing -> putStrLn "Parsing and typechecking produced no result."
    putStrLn ""
    where
        result :: (Maybe MTIR, [DMsg])
        result = runDF (parseCheck s (extend mtStdEnv bs))

        extend env [] = env
        extend env ((n,t):bs) =
            case enterIntTermSym n t NoSrcPos env of
                Left _ -> error "Extending MT Standard Environment failed!"
                Right (env', _) -> extend env' bs

        parseCheck :: String -> Env -> DF MTIR -- NOTE: Type signature added, because the inferred one was to general and implied constraint that are unavailable.
        parseCheck s env = do
            ast <- parse s
            failIfErrorsD
            c <- dToDF (chkCmd env (A.astCmd ast))
            return (MTIR {mtirCmd = c})


------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

tcErr :: String -> String -> a
tcErr = internalError "TypeChecker"
