{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         GodeGenMonad                                         *
*       Purpose:        Code Generation Monad                                *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Code generation monad. This module provides an abstraction for
-- code generation computations with support for generation of distinct
-- names, e.g. for labels.


-- ToDo:
-- o Consider diversion to numbered sections:
--   - divertTo :: Integer -> CG i a -> CG I a, and/or
--   - emitTo   :: Integer -> i -> CG i ()
--   But care is needed to handle "recursive" diversion properly.
--   For example, suppose the current section is 0. Then:
--     divertTo 1 (emitTo 0 i)
--   should have the same effect as just
--     emit i
--   This means that "divert" need to put back the current section
--   among the numbered ones in case there are any "subdivesions".

module CodeGenMonad (
    -- * Code generation computation
    CG,                 -- Abstract. Instances: Monad.
    emit,               -- :: i -> CG i x ()
    emitAux,            -- :: x -> CG i x ()
    divert,             -- :: CG i x a -> CG i x a
    newName,            -- :: CG i x Name
    runCG               -- :: CG i x a -> (a, [i], [x])
) where

-- Standard library imports
import Control.Applicative      -- Backwards compatibibility

-- HMTC module imports
import Name


------------------------------------------------------------------------------
-- Code generator state
------------------------------------------------------------------------------

data CGState i x
    = CGS {
        nxtNm :: Integer,       -- Next distinct name
        divs  :: [[i]],         -- Diversions
        sect  :: [i],           -- Current section
        aux   :: [x]            -- Auxiliary stream
    }


------------------------------------------------------------------------------
-- Code generator computation
------------------------------------------------------------------------------

-- | Code generation computation. Parameterised on the type of instructions
-- and additional auxiliary information. One use of the auxiliary information
-- is for additional separate output sections by instantiating with suitable
-- disjoint union type.

-- For example, Either can be used to implement prefix and suffix sections:
-- emitPfx i = emitAux (Left i), emitSfx = emitAUx (Right i) 
newtype CG i x a = CG (CGState i x -> (a, CGState i x))

unCG :: CG i x a -> (CGState i x -> (a, CGState i x))
unCG (CG f) = f


instance Functor (CG i x) where
    fmap f cg = CG $ \cgs ->
        let
            (a, cgs') = unCG cg cgs            
        in
            (f a, cgs')

    a <$ cg = CG $ \cgs ->
        let
            (_, cgs') = unCG cg cgs
        in
            (a, cgs')


instance Applicative (CG i x) where
    pure a = CG $ \cgs -> (a, cgs)

    cgf <*> cga = CG $ \cgs ->
        let
            (f, cgs')  = unCG cgf cgs
            (a, cgs'') = unCG cga cgs'
        in
            (f a, cgs'')


instance Monad (CG i x) where
    return = pure           -- Backwards compatibility

    cg >>= f = CG $ \cgs ->
        let
            (a, cgs') = unCG cg cgs
        in
            unCG (f a) cgs'


-- | Emit instruction
emit :: i -> CG i x ()
emit i = CG $ \cgs -> ((), cgs {sect = i : sect cgs})


-- | Emit auxiliary information
emitAux :: x -> CG i x ()
emitAux x = CG $ \cgs -> ((), cgs {aux = x : aux cgs})


-- | Divert output from sub-computation to separate section
divert :: CG i x a -> CG i x a
divert cg = CG $ \cgs -> 
    let
        (a, cgs') = unCG cg (cgs {sect = []})
    in
        (a, cgs' {divs = sect cgs' : divs cgs', sect = sect cgs})


-- | Generate a distinct name (within a run)
newName :: CG i x Name
newName = CG $ \cgs@(CGS {nxtNm = n}) -> ("#" ++ show n, cgs {nxtNm = n + 1})


-- | Run a code generation computation
runCG :: CG i x a -> (a, [i], [x])
runCG cg = 
    let
        (a, cgs') = unCG cg cgs0 
    in
        (a, joinSects (sect cgs' : divs cgs'), reverse (aux cgs'))
    where
        cgs0 = CGS {nxtNm = 0, divs = [], sect = [], aux = []}


joinSects :: [[i]] -> [i]
joinSects []     = []
joinSects (s:ss) = jsAux (joinSects ss) s
    where
        jsAux is []      = is
        jsAux is (i:ris) = jsAux (i:is) ris        
 

------------------------------------------------------------------------------
-- Code generator tests
------------------------------------------------------------------------------

cgTest :: CG String Int ()
cgTest = do
    l1 <- newName
    l2 <- newName
    emitAux 0
    emit ("LABEL " ++ l1)
    emit ("PUSH 0")
    divert cgTest1
    emit ("JUMPIFZ " ++ l2)
    emit ("JUMP " ++ l1)
    emitAux 10
    l3 <- newName
    emit ("LABEL " ++ l2)
    emit ("LABEL " ++ l3)

cgTest1 :: CG String Int ()
cgTest1 = do
    l1 <- newName
    emitAux 1
    divert cgTest2
    divert cgTest2
    l2 <- newName
    emitAux 11
    emit ("LABEL test1" ++ l1)
    emit ("STRING " ++ l2)

cgTest2 :: CG String Int ()
cgTest2 = do
    emitAux 2
    l1 <- newName
    emitAux 12
    emit ("LABEL test2" ++ l1)
