{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Env                                                  *
*       Purpose:        Environment (symbol table) with operations           *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Environment with operations. The module provides an abstract datatype
-- for representing environments (symbol tables), along with operations
-- for creating and extending environments and looking up symbols.
-- There are three kinds of symbols: term-level symbols, which are internal
-- or external, denoting entities like variables, constants, and procedures,
-- and type-level symbols, denoting types.

-- Note: the operations for extending the environment are responsible for
-- creating symbols. Creating symbols only in the context of an environment
-- allows assignment of scope levels and instance numbers for overloading
-- to be handled automatically.

module Env (
    -- Environments
    Env,                  -- Abstract.
    mkTopLvlEnv,          -- :: [(Name,Type)] -> [(Name,Type,ExtSymVal)] -> Env
    openMajScope,         -- :: Env -> Env
    openMinScope,         -- :: Env -> Env
    enterIntTermSym,      -- :: Name -> Type -> SrcPos -> Env
                          --    -> Either IntTermSym (Env, IntTermSym)
    enterOvrldIntTermSym, -- :: Name -> Type -> SrcPos -> Env
                          --    -> Either IntTermSym (Env, IntTermSym)
    lookupTypeSym,        -- :: Name -> Env -> Maybe TypeSym
    lookupTermSym,        -- :: Name -> Env -> Maybe TermSym
    lookupOvrldTermSym,   -- :: Name -> Env -> [TermSym]
    dummyTmS              -- :: Name -> TermSym
) where

-- Standard library imports
import Data.Maybe (isJust)
import Data.List (nub, sortBy, groupBy)

-- HMTC module imports
import Diagnostics (internalError)
import Name
import SrcPos
import ScopeLevel
import Type
import Symbol


-- | Environment (symbol table). Abstract.

data Env 
     = Env {
           envSL    :: ScopeLvl,        -- Current scope level
           envNIN   :: Int,             -- Next instance number
           envTpSs  :: [TypeSym],       -- Type symbols in scope
           envETmSs :: [ExtTermSym],    -- Ext. (top-level) symbols in scope
           envITmSs :: [IntTermSym]     -- Internal symbols in scope
       }
       deriving Show -- For testing/debugging


-- | Creates an initial, top-level, environment.
-- Arguments:
--
-- (1) List of name and type pairs for the type-level part of the top-level
-- environment; i.e. a /definition/ for each symbol.
-- Names must not be repeated: there is no overloading for type symbols.
-- 
-- (2) List of name and type pairs for the term-level part of the top-level
-- environment; i.e., a /declaration/ for each symbol.
-- Repeated names yield overloaded symbols, while each distinct name yields
-- a non-overloaded symbol.
--
-- Returns: A top-level environment.

mkTopLvlEnv :: [(Name, Type)] -> [(Name, Type, ExtSymVal)] -> Env
mkTopLvlEnv tpnts tmntvs =
    if length tpns == length (nub tpns) then
        Env {
            envSL    = topScopeLvl,
            envNIN   = 0,
            envTpSs  = tpss,
            envETmSs = etmss,
            envITmSs = []
        }
    else
        envErr "mkTopLvlEnv"
               "Type names must not be repeated (no overloading of types)."
    where
        tpns = map fst tpnts

        tpss = [ TypeSym {tpsName = n, tpsType = t, tpsSrcPos = NoSrcPos}
               | (n,t) <- tpnts
               ]

        etmss = concatMap
                    mkExtTermSyms
                    (groupBy (\(n1, _, _) (n2, _, _) -> n1 == n2)
                             (sortBy (\(n1, _, _) (n2, _, _) -> compare n1 n2)
                                     tmntvs))

        mkExtTermSyms :: [(Name, Type, ExtSymVal)] -> [ExtTermSym]
        mkExtTermSyms [(n, t, v)] =
            -- Singleton list: not an overloaded external symbol.
            [ ExtTermSym {
                 etmsName = n,
                 etmsInst = Nothing,
                 etmsType = t,
                 etmsVal  = v
              }
            ]
        mkExtTermSyms ntvs =
            -- Repeated name: overloaded external symbols
            [ ExtTermSym {
                 etmsName = n,
                 etmsInst = Just i,
                 etmsType = t,
                 etmsVal  = v
              }
            |
              ((n, t, v), i) <- zip ntvs [0..]
            ]


-- | Opens a new major (e.g. procedure/function) scope level.
openMajScope :: Env -> Env
openMajScope env = env {envSL = incMajScopeLvl (envSL env)}


-- | Opens a new minor (e.g. let) scope level.
openMinScope :: Env -> Env
openMinScope env = env {envSL = incMinScopeLvl (envSL env)}


-- | Enters a non-overloaded internal term-level symbol into an environment.
-- Enforces that non-overloaded symbols must be uniquely defined at each scope
-- level. Arguments:
--
-- (1) Name of symbol to be entered.
--
-- (2) Type of symbol to be entered.
--
-- (3) Source position of the declaration or definition of symbol to be
--     entered.
-- 
-- (4) The environment to extend.
--
-- On success (Right), returns:
--
-- (1) Extended environment.
-- 
-- (2) Copy of the new symbol.
--
-- On failure (Left), returns:
--
-- (1) The internal symbol with which there was a clash.

enterIntTermSym :: Name -> Type -> SrcPos -> Env
                   -> Either IntTermSym (Env, IntTermSym)
enterIntTermSym n t sp env@(Env {envSL = l, envITmSs = itmss}) =
    case redeclaredAux itmss of
        Just old -> Left old
        Nothing  -> Right (env {envITmSs = itms:itmss}, itms)
    where
        itms = IntTermSym {
                   itmsLvl    = l,
                   itmsInst   = Nothing,
                   itmsName   = n,
                   itmsType   = t,
                   itmsSrcPos = sp
               }

        -- Name clash with symbol at same scope level disallowed,
        -- regardless of whether the symbol is overloaded or not. 
        redeclaredAux [] = Nothing
        redeclaredAux (old@IntTermSym {itmsLvl = l', itmsName = n'} : itmss)
            | l' < l    = Nothing
            | n' == n   = Just old
            | otherwise = redeclaredAux itmss


-- | Enters a overloaded internal term-level symbol into an environment.
-- Enforces that there must be no non-overloaded symbol with the same name
-- at the current scope level; i.e., for a given scope level, there must either
-- be a single non-overloaded symbol with a particular name, or all symbols
-- having that name must be overloaded. Arguments:
--
-- (1) Name of symbol to be entered.
--
-- (2) Type of symbol to be entered.
--
-- (3) Source position of the declaration or definition of symbol to be
--     entered.
-- 
-- (4) The environment to extend.
--
-- On success (Right), returns:
--
-- (1) Extended environment.
-- 
-- (2) Copy of the new symbol.
--
-- On failure (Left), returns:
--
-- (1) The internal symbol with which there was a clash.

enterOvrldIntTermSym :: Name -> Type -> SrcPos -> Env
                        -> Either IntTermSym (Env, IntTermSym)
enterOvrldIntTermSym n t sp env@(Env {envSL = l, envNIN = i, envITmSs =itmss})=
    case redeclaredAux itmss of
        Just old -> Left old
        Nothing  -> Right (env {envNIN = i + 1, envITmSs = itms:itmss}, itms)
    where
        itms = IntTermSym {
                   itmsLvl    = l,
                   itmsInst   = Just i,
                   itmsName   = n,
                   itmsType   = t,
                   itmsSrcPos = sp
               }

        -- Name clash with non-overloaded symbol at same scope level
        -- disallowed.
        redeclaredAux [] = Nothing
        redeclaredAux (old@IntTermSym {itmsLvl = l', itmsInst = Just _}
                       : itmss)
            | l' < l    = Nothing
            | otherwise = redeclaredAux itmss
        redeclaredAux (old@IntTermSym {itmsLvl = l', itmsName = n'} : itmss)
            | l' < l    = Nothing
            | n' == n   = Just old
            | otherwise = redeclaredAux itmss


-- | Looks up a type-level symbol.
-- Arguments:
--
-- (1) Name of symbol to lookup.
-- 
-- (2) The environment in which to lookup the symbol.
-- 
-- On success, returns:
--
-- The symbol.

lookupTypeSym :: Name -> Env -> Maybe TypeSym
lookupTypeSym n env = ltpsAux (envTpSs env)
    where
        ltpsAux []                              = Nothing
        ltpsAux (tps : tpss) | tpsName tps == n = Just tps
                             | otherwise        = ltpsAux tpss


-- | Looks up a non-overloaded term-level symbol.
-- Later declarations (higher scope levels) shadow earlier ones.
-- Overloaded symbols are ignored. Thus this function should mainly be used
-- for finding symbols that cannot be overloaded.
-- Arguments:
--
-- (1) Name of symbol to lookup.
-- 
-- (2) The environment in which to lookup the symbol.
-- 
-- On success, returns:
--
-- The symbol.

lookupTermSym :: Name -> Env -> Maybe TermSym
lookupTermSym n (Env {envETmSs = etmss, envITmSs = itmss}) = litms itmss
    where
        litms []                     = letms etmss
        litms (itms : itmss)
            | isJust (itmsInst itms) = litms itmss
            | itmsName itms == n     = Just (Right itms)
            | otherwise              = litms itmss

        letms []                     = Nothing
        letms (etms : etmss)
            | isJust (etmsInst etms) = letms etmss
            | etmsName etms == n     = Just (Left etms)
            | otherwise              = letms etmss


-- | Looks up a possibly overloaded term-level symbol.
-- Returns all overloaded symbols of the given name and the first non-
-- overloaded symbol of that name, if any, most recently defined symbol
-- first. Arguments:
--
-- (1) Name of symbol to lookup.
-- 
-- (2) The environment in which to lookup the symbol.
-- 
-- Returns:
--
-- The symbols found.

lookupOvrldTermSym :: Name -> Env -> [TermSym]
lookupOvrldTermSym n (Env {envETmSs = etmss, envITmSs = itmss}) = litms itmss
    where
        litms []                 = letms etmss
        litms (itms : itmss)
            | itmsName itms == n = case itmsInst itms of
                                       Just _  -> Right itms : litms itmss
                                       Nothing -> [Right itms]
            | otherwise          = litms itmss

        letms []                 = []
        letms (etms : etmss)
            | etmsName etms == n = case etmsInst etms of
                                       Just _  -> Left etms : letms etmss
                                       Nothing -> [Left etms]
            | otherwise          = letms etmss


-- | Constructs a "dummy" (internal) term-level symbol to be used as a
-- placeholder for example where a lookup has failed but a term-level symbol
-- is needed.
--
-- (1) Name of the symbol.
--
-- Returns:
--
-- The dummy symbol.

dummyTmS :: Name -> TermSym
dummyTmS n = Right $
    IntTermSym {
        itmsLvl    = topScopeLvl,
        itmsName   = n,
        itmsInst   = Nothing,
        itmsType   = SomeType,
        itmsSrcPos = NoSrcPos
    }


envErr :: String -> String -> a
envErr = internalError "Env"


------------------------------------------------------------------------------
-- Test utilities
------------------------------------------------------------------------------

-- A faulty top-level environment
env1 = mkTopLvlEnv [("T1", Integer), ("T2", Integer), ("T1", Integer)] []


-- A top-level environment with overloaded symbols. 
env2 = mkTopLvlEnv
           [("T1", Integer), ("T2", Integer)]
           [("x", Integer, ESVInt 0),
            ("y", Integer, ESVInt 2),
            ("x", Integer, ESVInt 1),
            ("x", Integer, ESVInt 3),
            ("y", Integer, ESVInt 4),
            ("z", Integer, ESVInt 5)]

env3 = openMinScope env2

Right (env4, sym1) = enterIntTermSym "a" Boolean NoSrcPos env3
 
Right (env5, sym2) = enterIntTermSym "b" Integer NoSrcPos env4

res1 = enterIntTermSym "a" Boolean NoSrcPos env5

Right (env6, sym3) = enterOvrldIntTermSym "x" Integer NoSrcPos env5

Right (env7, sym4) = enterOvrldIntTermSym "z" Integer NoSrcPos env6

Right (env8, sym5) = enterOvrldIntTermSym "a" Boolean NoSrcPos (openMinScope env7)

Right (env9, sym6) = enterOvrldIntTermSym "z" Boolean NoSrcPos env8

res2 = enterIntTermSym "a" Integer NoSrcPos env9

res3 = lookupTypeSym "T1" env9

res4 = lookupTermSym "a" env9

res5 = lookupTermSym "b" env9

res6 = lookupTermSym "x" env9

res7 = lookupTermSym "y" env9

res8 = lookupTermSym "z" env9

res9 = lookupOvrldTermSym "a" env9

res10 = lookupOvrldTermSym "b" env9

res11 = lookupOvrldTermSym "x" env9

res12 = lookupOvrldTermSym "y" env9

res13 = lookupOvrldTermSym "z" env9

Right (env10, sym7) = enterIntTermSym "z" Integer NoSrcPos (openMinScope env9)

res14 = lookupOvrldTermSym "z" env10
