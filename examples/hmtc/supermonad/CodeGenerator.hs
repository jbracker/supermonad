{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         CodeGenerator                                        *
*       Purpose:        Generate Triangle Abstract Machine (TAM) code from   *
*                       MiniTriangle Intermediate Representation (MTIR)      *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | TAM Code Generator.

module CodeGenerator (
    genCode     -- :: MTIR -> D [TAMInst]
) where

import Control.Supermonad.Prelude

-- Standard library imports
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Array
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Map (Map)
import qualified Data.Map as M 

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import ScopeLevel
import Symbol
import Type
import MTIR
import TAMCode
import CodeGenMonad


------------------------------------------------------------------------------
-- Code generator environment
------------------------------------------------------------------------------

-- Maps internal term-level symbols either to a displacement (with the
-- proper base (SB, LB, or via static links) being determined by scope
-- level of symbol w.r.t. current scope level) or to a label (Name, for
-- local procedures and functions).
--
-- Note that the value of an *external* symbol is part of the symbol.
-- The value of internal symbols only become known *during* code generation.
-- That is why internal symbol do not have a value field, and why we need
-- an environment here to keep track of the mapping from internal symbols to
-- their values.
--
-- List used for simplicity. Could use a more efficient structure.
-- As symbols are used as keys (i.e., name + scope level), and symbols are
-- distinct within a scope, there is no need to use a structure and
-- lookup strategy that takes account of shadowing.

type CGEnv = [(IntTermSym, IntSymVal)]


data IntSymVal
    = ISVDisp MTInt     -- Displacment w.r.t. base of activation record
    | ISVLbl Name       -- Label for entry point of local function/procedure.
    deriving Show


emptyCGEnv = []


lookupISV :: IntTermSym -> CGEnv -> IntSymVal
lookupISV itms env =
    case lookup itms env of
        Just isv -> isv
        Nothing  -> cgErr "lookupISV"
                           ("Lookup of \"" ++ show itms ++ "\" failed!")


lookupLbl :: IntTermSym -> CGEnv -> Name
lookupLbl itms env =
    case lookupISV itms env of
        ISVLbl n -> n
        _        -> cgErr "lookupLbl"
                           ("Lookup of \""
                            ++ show itms
                            ++ "\" yielded a stack displacement, but label \
                               \expected.")


------------------------------------------------------------------------------
-- Top-level code generation functions
------------------------------------------------------------------------------

-- | TAM Code Generator.

-- Diagnostics computation as one may want to emit information or warnings.
-- However, there shouldn't really be any further errors to report.
genCode :: Bool -> MTIR -> D [TAMInst]
genCode optimize mtir = do
    let (_, code, _) = runCG (run mtir)
    return (if optimize then peepholeOpt code else code)


-- Type synonym for the TAM code generation monad
type TAMCG a = CG TAMInst () a


-- Type synonym for Major Scope Level
type MSL = Int


-- [One might consider Depth, Displacement, Size as synonyms for MTInt
-- in its various roles. But some of these should then really be declared
-- (and used) in TAMCode (e.g. type Addr should use Displacement.]


-- Generate code to run a complete program
run :: MTIR -> TAMCG ()
run (MTIR { mtirCmd = c}) = do
    execute topMajScopeLvl emptyCGEnv 0 c
    emit HALT


------------------------------------------------------------------------------
-- Code generation for commands
------------------------------------------------------------------------------

-- Generate code to execute a command.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The command
--
execute :: MSL -> CGEnv -> MTInt -> Command -> TAMCG ()
execute majl env n (CmdAssign {caVar = v, caVal = e}) = do
    evaluate majl env e         -- Have to evaluate e & v even if size of e is
    evaluate majl env v         -- 0 as there could be side effects. 
    emit (STOREIB (sizeOf (expType e)))
execute majl env n (CmdCall {ccProc = p, ccArgs = as}) = do
    mapM_ (evaluate majl env) as
    evaluate majl env p
    emit CALLI
execute majl env n (CmdSeq {csCmds = cs}) = executeSeq majl env n cs
execute majl env n (CmdIf {ciCondThens = ecs, ciMbElse = mbce}) = do
    lblOver <- newName
    mapM_ (executeCondThen majl env n lblOver) ecs
    case mbce of
        Nothing -> return ()
        Just ce -> execute majl env n ce
    emit (Label lblOver)
    where
        executeCondThen :: Int -> CGEnv -> MTInt -> Name
                           -> (Expression, Command)
                           -> TAMCG ()
        executeCondThen majl env n lblOver (e, c) = do
            lblElse <- newName
            evaluate majl env e
            emit (JUMPIFZ lblElse)
            execute majl env n c
            emit (JUMP lblOver)
            emit (Label lblElse)
execute majl env n (CmdWhile {cwCond = e, cwBody = c}) = do
    lblLoop <- newName
    lblCond <- newName
    emit (JUMP lblCond)
    emit (Label lblLoop)
    execute majl env n c
    emit (Label lblCond)
    evaluate majl env e
    emit (JUMPIFNZ lblLoop)
execute majl env n (CmdRepeat {crBody = c, crCond = e}) = do
    lblLoop <- newName
    emit (Label lblLoop)
    execute majl env n c
    evaluate majl env e
    emit (JUMPIFZ lblLoop)
execute majl env n (CmdLet {clDecls = ds, clBody = c}) = do
    (env', n') <- elaborateDecls majl env n ds
    execute majl env' n' c
    emit (POP 0 (n' - n))


-- Generate code to execute a sequence of commands.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The commands
--
executeSeq :: MSL -> CGEnv -> MTInt -> [Command] -> TAMCG ()
executeSeq majl env n []     = return ()
executeSeq majl env n (c:cs) = do
    execute majl env n c
    executeSeq majl env n cs


------------------------------------------------------------------------------
-- Code generation for declarations
------------------------------------------------------------------------------

-- Elaborate declarations and generate initialization code and code for
-- functions and procedures. Storage for the variable is allocated on the
-- top of the stack.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth
--
elaborateDecls :: MSL -> CGEnv -> MTInt -> [Declaration] -> TAMCG (CGEnv,MTInt)
elaborateDecls majl env n ds = do
     (env', n') <- extendEnv env n ds
     mapM_ (elaborateDecl majl env') ds
     return (env', n')


-- Extend environment according to declarations.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth
--
extendEnv :: CGEnv -> MTInt -> [Declaration] -> TAMCG (CGEnv, MTInt)
extendEnv env n [] = return (env, n)
extendEnv env n (DeclConst {dcConst = x} : ds) = do
    let s = sizeOf (rfcdType (itmsType x))
    extendEnv ((x, ISVDisp n) : env) (n + s) ds
extendEnv env n (DeclVar {dvVar = x} : ds) = do
    let s = sizeOf (rfcdType (itmsType x))
    extendEnv ((x, ISVDisp n) : env) (n + s) ds
extendEnv env n (DeclFun {dfFun = f} : ds) = do
    fn <- newSuffixedName (itmsName f)
    extendEnv ((f, ISVLbl fn) : env) n ds
extendEnv env n (DeclProc {dpProc = p} : ds) = do
    pn <- newSuffixedName (itmsName p)
    extendEnv ((p, ISVLbl pn) : env) n ds


-- Elaborate single declaration.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) The declaration
--
elaborateDecl :: MSL -> CGEnv -> Declaration -> TAMCG ()
elaborateDecl majl env (DeclConst {dcVal = e}) =
    evaluate majl env e
elaborateDecl majl env (DeclVar {dvVar = x, dvMbVal = Nothing}) =
    emit (LOADLB 0 (sizeOf (rfcdType (itmsType x))))
elaborateDecl majl env (DeclVar {dvMbVal = Just e}) =
    evaluate majl env e
elaborateDecl majl env (DeclFun {dfFun = f, dfArgs = as, dfBody = e}) = 
    divert $ do
        let fn = lookupLbl f env
        emit (Label fn)
        let (env', n) = extendEnvArgs env as
        evaluate (majl + 1) env' e
        emit (RETURN (sizeOf (retType (itmsType f))) n)
elaborateDecl majl env (DeclProc {dpProc = p, dpArgs = as, dpBody = c}) =
    divert $ do
        let pn = lookupLbl p env
        emit (Label pn)
        let (env', n) = extendEnvArgs env as
        execute (majl + 1) env' lrs c
        emit (RETURN 0 n)


-- Extend environment according to argument declarations. Arguments
-- are allocated by the caller and are addressed with negative offsets from
-- local base (LB).
-- Arguments:
--
-- (1) Enviornment
--
-- (2) The argument declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) Combined size of all passed arguments.
--
extendEnvArgs :: CGEnv -> [IntTermSym] -> (CGEnv, MTInt)
extendEnvArgs env as = (eeaAux env (-n) as, n)
    where
        n = sum [ argSize a | a <- as ]
        
        eeaAux env _ []     = env
        eeaAux env d (a:as) = eeaAux ((a, ISVDisp d) : env) (d + argSize a) as

        argSize a = sizeOf (rfcdType (itmsType a))


------------------------------------------------------------------------------
-- Code generation for expressions
------------------------------------------------------------------------------

-- Generate code to evaluate an expression.
-- Result is left on the top of the stack.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) The expression
--
evaluate :: Int -> CGEnv -> Expression -> TAMCG ()
evaluate majl env (ExpLitBool {elbVal = b}) =
    emit (LOADL (tamRepBool b))
evaluate majl env (ExpLitInt {eliVal = v}) =
    emit (LOADL v)
evaluate majl env (ExpLitChr {elcVal = c}) =
    emit (LOADL (tamRepMTChr c))
evaluate majl env (ExpExtRef {eerVal = l}) = do
    emit (LOADL 0)
    emit (LOADCA l)
evaluate majl env (ExpVar {evVar = itms}) =
    case lookupISV itms env of
        ISVDisp d ->
            address majl vl d
        ISVLbl l -> do
            staticLink majl vl
            emit (LOADCA l)
    where
        vl = majScopeLvl (itmsLvl itms)
evaluate majl env (ExpDeref {edArg = e, expType = t}) = do
    evaluate majl env e         -- Have to evaluate e even if size 0 as there
                                -- could be side effects.
    emit (LOADIB (sizeOf t))
evaluate majl env (ExpApp {eaFun = f, eaArgs = as}) = do
    mapM_ (evaluate majl env) as
    evaluate majl env f
    emit CALLI     
evaluate majl env (ExpCond {ecCond = e1, ecTrue = e2, ecFalse = e3}) = do
    lblElse <- newName
    lblOver <- newName
    evaluate majl env e1
    emit (JUMPIFZ lblElse)
    evaluate majl env e2
    emit (JUMP lblOver)
    emit (Label lblElse)
    evaluate majl env e3
    emit (Label lblOver)
evaluate majl env (ExpAry {eaElts = es}) =
    mapM_ (evaluate majl env) es
evaluate majl env (ExpIx  {eiAry = a, eiIx = i, expType = t}) = do
    let ta = rfcdType (expType a)
    callIxError <- newName
    over        <- newName
    evaluate majl env a
    evaluate majl env i
    emit (LOAD (ST (-1)))
    emit (LOADL 0)
    emit LSS
    emit (JUMPIFNZ callIxError)
    emit (LOAD (ST (-1)))
    emit (LOADL (arySize ta))
    emit LSS
    emit (JUMPIFNZ over)
    emit (Label callIxError)
    emit (CALL "ixerror")       -- Bad ix on top of stk; halts TAM, no return!
    emit (Label over)
    emit (LOADL (sizeOf (eltType ta)))
    emit MUL
    emit ADD
evaluate majl env (ExpRcd {erFldDefs = fds}) =
    mapM_ (evaluate majl env . snd) fds
evaluate majl env (ExpPrj {epRcd = r, epFld = f, expType = t}) = do
    let tr = rfcdType (expType r)
    evaluate majl env r
    emit (LOADL (fldOffset f tr))
    emit ADD


------------------------------------------------------------------------------
-- Code generation for variable access and computation of static links
------------------------------------------------------------------------------

-- Generate code to push address of a variable onto the stack.
-- Arguments:
--
-- (1) Major scope level of code
--
-- (2) Major scope level of variable
--
-- (3) Displacement of the variable
--
address :: Int -> Int -> MTInt -> TAMCG ()
address cl vl d
    | vl == topMajScopeLvl =
        emit (LOADA (SB d))
    | cl == vl =
        emit (LOADA (LB d))
    | cl > vl  = do
        emit (LOAD (LB sld))
        emitN (cl - vl - 1) (LOADI sld)
        emit (LOADL d)
        emit ADD
    | otherwise =
        cgErr "address" "Attempt to access variable from inner scope."


-- Generate code to push static link for callee onto the stack.
-- Arguments:
--
-- (1) Major scope level of caller
--
-- (2) Major scope level of callee
--
staticLink :: Int -> Int -> TAMCG ()
staticLink crl cel
    | cel == topMajScopeLvl =
        emit (LOADL 0)
    | crl == cel =
        emit (LOADA (LB 0))
    | crl > cel  = do
        emit (LOAD (LB sld))
        emitN (crl - cel - 1) (LOADI sld)
    | otherwise =
        cgErr "staticLink" "Attempt to call procedure/function in inner scope."


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

emitN :: Int -> TAMInst -> TAMCG ()
emitN n i | n <= 0    = return ()
          | otherwise = emit i >> emitN (n - 1) i


newSuffixedName :: Name -> TAMCG Name
newSuffixedName sfx = do
    nm <- newName
    return (nm ++ "_" ++ sfx)


------------------------------------------------------------------------------
-- TAM stack frame layout
------------------------------------------------------------------------------

-- Static link displacement
sld :: MTInt
sld = 0

-- Dynamic link displacement
dld :: MTInt
dld = 1

-- Return address displacement
rad :: MTInt
rad = 2

-- Size of link and return address area
lrs :: MTInt
lrs = 3 

------------------------------------------------------------------------------
-- TAM data representation
------------------------------------------------------------------------------

-- The TAM is designed on the assumption that basic MiniTriangle types
-- (Boolean, Integer, Character) as well as reference types all can
-- be represented by a single machine word, an MTInt. Further, each TAM
-- stack element is a single machine word. These assumptions are in
-- many cases hard-coded in the TAM code generator.

-- Size of TAM represntation of MiniTriangle types
sizeOf :: Type -> MTInt
sizeOf SomeType  = cgErr "sizeOf" sizeOfErrMsgSomeType
sizeOf Void      = 0
sizeOf Boolean   = 1
sizeOf Character = 1
sizeOf Integer   = 1
sizeOf (Src _)   = 1
sizeOf (Snk _)   = 1
sizeOf (Ref _)   = 1
-- Size of array of length 0 is always 0, irrespective of element type.
-- In particular, do not want to call sizeOf recursively if the element
-- type is "SomeType".
sizeOf (Ary t n) = if n == 0 then 0 else sizeOf t * n
sizeOf (Rcd fts) = sum (map (sizeOf . snd) fts)
-- If support for passing fun/proc as parameters added, this should be
-- the size of a "closure" which is represented by static link & address.
sizeOf (Arr _ _) = cgErr "sizeOf" sizeOfErrMsgArr


-- Offset of record field
fldOffset :: Name -> Type -> MTInt
fldOffset f (Rcd fts) = foAux 0 fts
    where
        foAux offs []                          = offs
        foAux offs ((f', t) : fts) | f == f'   = offs
                                   | otherwise = foAux (offs + sizeOf t) fts
fldOffset f t =
    cgErr "fldOffset"
          ("Attempt to compute offset of field \"" 
           ++ f ++ "\" for non-record type \"" ++ show t ++ "\"." )


sizeOfErrMsgSomeType :: String
sizeOfErrMsgSomeType =
    "\"SomeType\" has no run-time representation and thus no size; Attempt \
    \to generate code for ill-typed program?"


sizeOfErrMsgArr :: String
sizeOfErrMsgArr =
    "Functions as data is not presently supported; function types thus has \
    \no run-time representation and no size."


tamRepBool :: Bool -> MTInt
tamRepBool False = 0
tamRepBool True  = 1


-- Note that there is a check in place to ensure this is a total function.
-- See "isMTchr" in "Type".

tamRepMTChr :: MTChr -> MTInt
tamRepMTChr c = fromIntegral (fromEnum c)


------------------------------------------------------------------------------
-- Simple peephole optimizer
------------------------------------------------------------------------------

-- This peephole optimizer is designed to complement the code generator.
-- Knowing that the peephole optimizer takes care of replacing short sequences
-- of instructions with improved sequences, the code generator can be kept
-- simple by choosing the instructions without considering the context. For
-- example, it can generate a LOADIB (block load) regardless of the number of
-- words, leaving it to the peephole optimizer to pick a better instruction or
-- even eliminate it and a preceding address calculation altogether should the
-- block be of size 0 (e.g. empty array).
--
-- Some additional optimizations, such as optimizing NOT;JUMPIFZ to JUMPIFNZ
-- are also done. However, note that attempts to simplify arithmetic or
-- taking advantage seemingly obvious opportunities such as optimizing NOT;NOT
-- to nothing are mostly not done at present. Generally, most such
-- optimizations are probably more at home at a higher level, and some could
-- be subtly wrong: e.g. NOT;NOT is not the identity on  the top stack element
-- unless it is guaranteed that this element is a representation of a Boolean.
--
-- Additionally, the optimizer carries out simple control-flow optimizations.
-- A label l immediately followed by an unconditional jump to a label l',
-- l' /= l, is an alias for l'. The main objective of the control flow
-- optimization is to eliminate label aliases by redirecting jumps to aliases
-- directly to the aliased label, and then eliminating any label that no
-- longer can be the target of any jump, along with instructions that
-- manifestly have become unreachable.
--
-- The optimizer is run twice. This should cover most optimization
-- opportunities in practice. In particular, enough to fully optimize examples
-- like:
--
--     if false then if false then putint(1) else putint(2) else putint(3)
-- 
-- (any number of "if false") fully. (After first phase, there will
-- be residual dead branches because there initially were jumps targetting
-- those labels). Alternatively, one could iterate until a fixed-point is
-- reached.

peepholeOpt :: [TAMInst] -> [TAMInst]
peepholeOpt tis = peepholeOpt' (peepholeOpt' tis)

peepholeOpt' :: [TAMInst] -> [TAMInst]
peepholeOpt' tis = poAux [] tis'
    where
        -- window: Largest number of TAM instructions in a pattern in poAux
        window = 3 :: Int

        -- wlo: "window less one": the number of steps we need to back up
        -- after a reduction to consider impact of change within window.
        wlo = window - 1

        tis' = elimUCCJ tis

        -- lam: label alias map; tls: targeted labels
        (lam, tls) = atAux [] [] tis' 

        -- Final taret label for a jump to given label 
        target l = M.findWithDefault l l lam

        -- ptis: Preceding TAM instructions in reverse order
        poAux ptis (LOADA a : LOADI d : tis)  =
            poBack wlo ptis (LOAD (addDisp a d) : tis)
        poAux ptis (LOADA a : STOREI d : tis) =
            poBack wlo ptis (STORE (addDisp a d) : tis)
        poAux ptis (LOADL n : ADD : LOADI d : tis)  =
            poBack wlo ptis (LOADI (d + n) : tis)
        poAux ptis (LOADL n : ADD : STOREI d : tis) =
            poBack wlo ptis (STOREI (d + n) : tis)
        poAux ptis (LOADL n1 : LOADL n2 : tis) | n1 == n2 =
            poBack wlo ptis ((LOADLB n1 2) : tis)
        poAux ptis (LOADL m1 : LOADLB m2 n : tis) | m1 == m2 =
            poBack wlo ptis ((LOADLB m1 (n + 1)) : tis)
        poAux ptis (LOADLB m1 n : LOADL m2 : tis) | m1 == m2 =
            poBack wlo ptis ((LOADLB m1 (n + 1)) : tis)
        poAux ptis (LOADLB m1 n1 : LOADLB m2 n2 : tis) | m1 == m2 =
            poBack wlo ptis ((LOADLB m1 (n1 + n2)) : tis)
        poAux ptis (LOADLB m 0 : tis) =
            poBack wlo ptis tis
        poAux ptis (LOADLB m 1 : tis) =
            poBack wlo ptis (LOADL m : tis)
        poAux ptis (LOADLB m n1 : POP 0 n2 : tis)
            | n1 >= n2  = poBack wlo ptis (LOADLB m (n1 - n2) : tis)
            | otherwise = poBack wlo ptis (POP 0 (n2 - n1) : tis)
        poAux ptis (LOADIB 0 : tis) =
            poBack wlo ptis (POP 0 1 : tis)
        poAux ptis (LOADIB 1 : tis) =
            poBack wlo ptis (LOADI 0 : tis)
        poAux ptis (LOADIB n1 : POP 0 n2 : tis)
            | n1 >= n2  = poBack wlo ptis (LOADIB (n1 - n2) : tis)
            | otherwise = poBack wlo ptis (POP 0 (n2 - n1 + 1) : tis)
        poAux ptis (STOREIB 0 : tis) =
            poBack wlo ptis (POP 0 1 : tis)
        poAux ptis (STOREIB 1 : tis) =
            poBack wlo ptis (STOREI 0 : tis)
        poAux ptis (POP m 0 : tis) =
            poBack wlo ptis tis
        poAux ptis (POP m1 n1 : POP m2 n2 : tis) | m1 == m2 =
            poBack wlo ptis (POP m1 (n1 + n2) : tis)
        poAux ptis (ti : POP 0 n : tis) | n > 0 =
            case eliminable ti of
                Nothing -> poAux (ti : ptis) (POP 0 n : tis)
                Just m  -> poBack wlo ptis (POP 0 (n - 1 + m) : tis)
        poAux ptis (LOADL 0 : ADD : tis) =
            poBack wlo ptis tis
        poAux ptis (LOADL 1 : MUL : tis) =
            poBack wlo ptis tis
        poAux ptis (LOADL 0 : MUL : tis) =
            poBack wlo ptis (POP 0 1 : LOADL 0 : tis)
        poAux ptis (LOADL 0 : LOADCA l : CALLI : tis) =
            poBack wlo ptis (CALL l : tis)
        poAux ptis (LOADLB 0 n : LOADCA l : CALLI : tis)
            | n == 1 = poBack wlo ptis (CALL l : tis)
            | n == 2 = poBack wlo ptis (LOADL 0 : (CALL l : tis))
            | n >  2 = poBack wlo ptis (LOADLB 0 (n-1) : (CALL l : tis))
        poAux ptis (li@(Label l) : tis)
            | l `S.notMember` tls = poBack wlo ptis tis
            | otherwise         = poAux (li : ptis) tis
        poAux ptis (JUMP l : tis) =
            case findLbl tis of
                tis1@(Label l1 : _) | l1 == l' -> poBack wlo ptis tis1
                tis1                           -> poAux (JUMP l' : ptis) tis1 
            where
                l' = target l
        poAux ptis (LOADCA l : tis) =
            poAux (LOADCA (target l) : ptis) tis
        poAux ptis (NOT : JUMPIFZ l : tis) =
            poBack wlo ptis (JUMPIFNZ l : tis)
        poAux ptis (NOT : JUMPIFNZ l : tis) =
            poBack wlo ptis (JUMPIFZ l : tis)
        poAux ptis (JUMPIFZ l : Label l' : tis) | l == l' =
            poBack wlo ptis (POP 0 1 : Label l' : tis)
        poAux ptis (JUMPIFNZ l : Label l' : tis) | l == l' =
            poBack wlo ptis (POP 0 1 : Label l' : tis)
        poAux ptis (JUMPIFZ l : tis) =
            poAux (JUMPIFZ (target l) : ptis) tis
        poAux ptis (JUMPIFNZ l : tis) =
            poAux (JUMPIFNZ (target l) : ptis) tis
        poAux ptis (CALL l : tis) =
            case knownSeq l of
                Nothing   -> poAux (CALL (target l) : ptis) tis
                Just tis' -> poBack wlo ptis (tis' ++ tis)
        poAux ptis (ti : tis) = poAux (ti : ptis) tis
        poAux ptis []         = reverse ptis

        poBack _ [] tis = poAux [] tis
        poBack n tiptis@(ti : ptis) tis
            | n > 0     = poBack (n - 1) ptis (ti : tis)
            | otherwise = poAux tiptis tis 
                
        findLbl [] = []
        findLbl litis@(Label l : tis)
            | l `S.notMember` tls = findLbl tis
            | otherwise         = litis
        findLbl (_ : tis)       = findLbl tis

        addDisp :: Addr -> MTInt -> Addr
        addDisp (SB d) d' = SB (d + d')
        addDisp (LB d) d' = LB (d + d')
        addDisp (ST d) d' = ST (d + d')

        -- Checks if an instruction can be eliminated if the top stack element
        -- is discarded (e.g. by a "POP 0 n" instruction for n > 0).
        -- Returns Nothing if not (e.g. the instruction may have effects).
        -- Returns the number of further stack elements to discard if yes
        -- (i.e., how many arguments the instruction has).
        eliminable :: TAMInst -> Maybe MTInt
        eliminable (LOADL _)  = Just 0
        eliminable (LOADCA _) = Just 0
        eliminable (LOAD _)   = Just 0
        eliminable (LOADA _)  = Just 0
        eliminable (LOADI _)  = Just 1
        eliminable ADD        = Just 2
        eliminable SUB        = Just 2
        eliminable MUL        = Just 2
        eliminable DIV        = Just 2
        eliminable NEG        = Just 1
        eliminable LSS        = Just 2
        eliminable EQL        = Just 2
        eliminable GTR        = Just 2
        eliminable AND        = Just 2
        eliminable OR         = Just 2
        eliminable NOT        = Just 1
        eliminable _          = Nothing

        -- Equivalent TAM sequences for "known" library functions for simple
        -- form of inlining. 
        knownSeq :: Name -> Maybe [TAMInst]
        knownSeq "add" = Just [ADD]
        knownSeq "sub" = Just [SUB]
        knownSeq "mul" = Just [MUL]
        knownSeq "div" = Just [DIV]
        knownSeq "neg" = Just [NEG]
        knownSeq "lt"  = Just [LSS]
        knownSeq "le"  = Just [GTR, NOT]
        knownSeq "eq"  = Just [EQL]
        knownSeq "ne"  = Just [EQL, NOT]
        knownSeq "ge"  = Just [LSS, NOT]
        knownSeq "gt"  = Just [GTR]
        knownSeq "and" = Just [AND]
        knownSeq "or"  = Just [OR]
        knownSeq "not" = Just [NOT]
        knownSeq _     = Nothing
        
        -- Eliminate conditional jumps that manifestly are not conditional
        elimUCCJ :: [TAMInst] -> [TAMInst]
        elimUCCJ [] = []
        elimUCCJ (LOADL n : JUMPIFZ l : tis)
            | n == 0    = JUMP l : elimUCCJ tis
            | otherwise = elimUCCJ tis
        elimUCCJ (LOADL n : JUMPIFNZ l : tis)
            | n /= 0    = JUMP l : elimUCCJ tis
            | otherwise = elimUCCJ tis
        elimUCCJ (JUMPIFZ l : Label l' : tis)
            | l == l' = POP 0 1 : Label l' : elimUCCJ tis
        elimUCCJ (JUMPIFNZ l : Label l' : tis)
            | l == l' = POP 0 1 : Label l' : elimUCCJ tis
        elimUCCJ (ti : tis) = ti : elimUCCJ tis

        -- Compute label Alias map and set of Targeted labels
        atAux :: [(Name,Name)] -> [Name] -> [TAMInst]
                 -> (Map Name Name, Set Name)
        atAux lam tls [] = (lam', tls')
            where
                lam' = M.fromList (filter (\(a,b) -> a /= b) lam)
                tls' = S.fromList tls S.\\ M.keysSet lam'
        -- l is effectively a jump to l', creating alias and making l' a target
        atAux lam tls (Label l : Label l' : tis) = atAux (extend (l, l') lam)
                                                         (l' : tls)
                                                         (Label l' : tis)
        atAux lam tls (Label l : JUMP l' : tis)  = atAux (extend (l, l') lam)
                                                         (l' : tls)
                                                         tis
        atAux lam tls (LOADCA l : tis)           = atAux lam (l : tls) tis
        atAux lam tls (JUMP l : tis)             = atAux lam (l : tls) tis
        atAux lam tls (JUMPIFZ l : tis)          = atAux lam (l : tls) tis
        atAux lam tls (JUMPIFNZ l : tis)         = atAux lam (l : tls) tis
        atAux lam tls (CALL l : tis)             = atAux lam (l : tls) tis
        atAux lam tls (_ : tis)                  = atAux lam tls tis

        -- Invariant: x \in dom(m) \cap ran(m) => m(x) = x
        -- Precondition: a \notin dom(m)
        extend :: Eq a => (a,a) -> [(a,a)] -> [(a,a)]
        extend (a, b) m = m'
            where
                (b'', m') = extAux b'' [] a b m
        
                extAux b'' m' a b [] = (b, (a, b'') : m')
                extAux b'' m' a b ((a1, b1) : m) = 
                    case (b == a1, b1 == a) of
                        (False, False) -> extAux b'' ((a1, b1) : m') a b m
                        (False, True)  -> extAux b'' ((a1, b'') : m') a b m
                        (True,  _)     -> extAux b'' ((a1, b1) : m') a b1 m


gen1 :: Int -> [TAMInst]
gen1 n = mkJump 1 : mkLbl (n + 1) : LOADL 0 : HALT : gen1aux n
    where
        gen1aux 0 = []
        gen1aux n = mkLbl n : mkJump (n + 1) : gen1aux (n - 1)

gen2 :: Int -> [TAMInst]
gen2 n = mkJump 1 : mkLbl (n + 1) : gen2aux n
    where
        gen2aux 0 = []
        gen2aux n = mkLbl n : mkJump (n + 1) : gen2aux (n - 1)



mkLbl n = Label ("#" ++ show n)

mkJump n = JUMP ("#" ++ show (n :: Int))

------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

cgErr :: String -> String -> a
cgErr = internalError "CodeGenerator"
