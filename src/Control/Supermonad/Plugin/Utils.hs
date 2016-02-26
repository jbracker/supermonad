
-- | Provides all kinds of functions that are needed by the plugin.
module Control.Supermonad.Plugin.Utils (
  -- * Type inspection
    collectTopTyCons
  , collectTopTcVars
  , collectTyVars
  , mkTcVarSubst
  , splitTyConApps
  , isGroundUnaryTyCon
  -- * General Utilities
  , skolemVarsBindFun
  , eqTyVar, eqTyVar'
  , eqTyCon
  , isAmbiguousType
  , getTyConWithArgKinds
  , applyTyCon
  , atIndex
  , associations
  , subsets
  , removeDup
  , lookupBy
  , allM, anyM
  , fromLeft, fromRight
  ) where

import Data.Maybe ( listToMaybe, catMaybes )
import Data.List ( find )
import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad ( forM )
import Control.Arrow ( second )

import Type
  ( Type, TyVar, TvSubst
  , getTyVar_maybe
  , tyConAppTyCon_maybe
  , splitTyConApp_maybe, splitFunTy_maybe, splitAppTy_maybe
  , getEqPredTys_maybe
  , splitAppTys
  , mkTyConTy, mkTyVarTy, mkAppTys
  , mkTopTvSubst
  , eqType )
import TyCon ( TyCon, tyConArity, tyConKind )
import Var ( tyVarKind )
import TcType ( isAmbiguousTyVar )
import Kind ( Kind, splitKindFunTys )
import Unify ( BindFlag(..) )
import InstEnv ( instanceBindFun )
import TcPluginM ( TcPluginM, newFlexiTyVar )

-- -----------------------------------------------------------------------------
-- Constraint and type inspection
-- -----------------------------------------------------------------------------

-- | Retrieve the type constructors at top level involved in the given types.
--   If there are type constructors nested within the type they are ignored.
--
--   /Example/:
--
-- >>> collectTopTyCons [Maybe (Identity ())]
-- { Maybe }
collectTopTyCons :: [Type] -> Set TyCon
collectTopTyCons tys = S.fromList $ catMaybes $ fmap tyConAppTyCon_maybe tys

-- | Retrieve the type constructor variables at the top level involved in the
--   given types. If there are nested type variables they are ignored.
--   There is no actual check if the returned type variables are actually type
--   constructor variables.
--
--   /Example/:
--
-- >>> collectTopTcVars [m a b, Identity c, n]
-- { m, n }
collectTopTcVars :: [Type] -> Set TyVar
collectTopTcVars tys = S.fromList $ catMaybes $ fmap (getTyVar_maybe . fst . splitAppTys) tys

-- | Try to collect all type variables in a given expression.
--   Does not work for Pi or ForAll types.
--   If the given type is not supported an empty set is returned.
collectTyVars :: Type -> Set TyVar
collectTyVars t =
  case getTyVar_maybe t of
    Just tv -> S.singleton tv
    Nothing -> case splitTyConApp_maybe t of
      Just (_tc, args) -> S.unions $ fmap collectTyVars args
      Nothing -> case splitFunTy_maybe t of
        Just (ta, tb) -> collectTyVars ta `S.union` collectTyVars tb
        Nothing -> case splitAppTy_maybe t of
          Just (ta, tb) -> collectTyVars ta `S.union` collectTyVars tb
          Nothing -> case getEqPredTys_maybe t of
            Just (_r, ta, tb) -> collectTyVars ta `S.union` collectTyVars tb
            Nothing -> S.empty

-- | Create a substitution that replaces the given type variables with their
--   associated type constructors.
mkTcVarSubst :: [(TyVar, TyCon)] -> TvSubst
mkTcVarSubst substs = mkTopTvSubst $ fmap (second mkTyConTy) substs

-- | Split type constructor applications into their type constructor and arguments. Only
--   keeps those in the result list where this split actually worked.
splitTyConApps :: [Type] -> [(TyCon, [Type])]
splitTyConApps = catMaybes . fmap splitTyConApp_maybe

-- | Check if the given type is a type constructor that is partially applied
--   such that it now is a unary type constructor.
isGroundUnaryTyCon :: Type -> Bool
isGroundUnaryTyCon t = case splitTyConApp_maybe t of
  Just (tc, args) -> tyConArity tc == length args + 1
  Nothing -> False

-- -----------------------------------------------------------------------------
-- General utilities
-- -----------------------------------------------------------------------------

-- | Override the standard bind flag of a given list of variables to 'Skolem'.
--   The standard bind flad is determined using 'instanceBindFun'.
--   This can be used to keep 'tcUnifyTys' from unifying the given variables
--   and to view them as constants.
skolemVarsBindFun :: [TyVar] -> TyVar -> BindFlag
skolemVarsBindFun tvs var = case find (var ==) tvs of
  Just _ -> Skolem
  Nothing -> instanceBindFun var

-- | Check if both types contain type variables and if those type
--   variables are equal.
eqTyVar :: Type -> Type -> Bool
eqTyVar ty ty' = case getTyVar_maybe ty of
  Just tv -> eqTyVar' tv ty'
  _ -> False

-- | Check if the given type constrains a type variable and it is equal to
--   the given type variable.
eqTyVar' :: TyVar -> Type -> Bool
eqTyVar' tv ty = case getTyVar_maybe ty of
  Just tv' -> tv == tv'
  Nothing  -> False

-- | Checks if the given type constructors equals the given type.
-- TODO: Test!
eqTyCon :: TyCon -> Type -> Bool
eqTyCon tc = eqType (mkTyConTy tc)

-- | Get the element of a list at a given index (If that element exists).
atIndex :: [a] -> Int -> Maybe a
atIndex xs i = listToMaybe $ drop i xs

-- | Checks if the given type is an ambiguous type variable.
isAmbiguousType :: Type -> Bool
isAmbiguousType ty = maybe False isAmbiguousTyVar $ getTyVar_maybe ty

-- | Takes a type that is considered to be a unary type constructor.
--   Tries to get the base type constructor within this, for example:
--
-- >>> getTyConWithArgKinds (StateT String)
-- (Left StateT, [*, *])
--
-- >>> getTyConWithArgKinds m
-- (Right m, [*])
--
-- >>> getTyConWithArgKinds (p s)
-- (Right p, [*, *]) -- Assuming the kind of s is *. case getEqPredTys_maybe t of
--
-- >>> getTyConWithArgKinds Identity
-- (Left Identity, [*])
getTyConWithArgKinds :: Type -> (Either TyCon TyVar, [Kind])
getTyConWithArgKinds t = case getTyVar_maybe tcTy of
  Just tv -> (Right tv, fst $ splitKindFunTys $ tyVarKind tv)
  Nothing -> case tyConAppTyCon_maybe tcTy of
    Just tc -> (Left tc, fst $ splitKindFunTys $ tyConKind tc)
    Nothing -> error "getTyConWithArity: Type does not contain a type constructor or variable."
  where (tcTy, _args) = splitAppTys t

-- | Applies the given type constructor or type constructor variable to enough
--   correctly kinded variables to make it a partially applied unary type
--   constructor. The partially applied unary type constructor is returned
--   together with the variables that were applied to it.
--
--   Will return 'Nothing' if there are to few kind arguments. It's supposed to be
--   used in conjunction with the first part of 'getCurrentPolymonad'.
applyTyCon :: (Either TyCon TyVar, [Kind]) -> TcPluginM (Maybe (Type, [TyVar]))
applyTyCon (_    , []) = return Nothing
applyTyCon (eTcTv, ks) = do
  let ks' = init ks
  tyVarArgs <- forM ks' newFlexiTyVar
  let t = either mkTyConTy mkTyVarTy eTcTv
  return $ Just (mkAppTys t $ fmap mkTyVarTy tyVarArgs, tyVarArgs)

-- | Takes a list of keys and all of their possible values and returns a list
--   of all possible associations between keys and values
--
--   /Examples/:
--
-- >>> associations [('a', [1,2,3]), ('b', [4,5])]
-- [ [('a', 1), ('b', 4)], [('a', 1), ('b', 5)]
-- , [('a', 2), ('b', 4)], [('a', 2), ('b', 5)]
-- , [('a', 3), ('b', 4)], [('a', 3), ('b', 5)] ]
associations :: [(key , [value])] -> [[(key, value)]]
associations [] = [[]]
associations ((_x, []) : _xys) = []
associations ((x, y : ys) : xys) = fmap ((x, y) :) (associations xys) ++ associations ((x, ys) : xys)

-- | Generates the set of all subsets of a given set.
subsets :: (Ord a) => Set a -> Set (Set a)
subsets s = case S.size s of
  0 -> S.singleton S.empty
  _ -> let (x, s') = S.deleteFindMin s
           subs = subsets s'
       in S.map (S.insert x) subs `S.union` subs

-- | Efficient removal of duplicate elements in O(n * log(n)).
--   The result list is ordered in ascending order.
removeDup :: (Ord a) => [a] -> [a]
removeDup = S.toAscList . S.fromList

-- | Exactly like 'lookup'. Searches the list for the entry with the right key
--   and returns the associated value if an entry is found. Uses a custom
--   function to check equality.
lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy _eq _x [] = Nothing
lookupBy eq x ((y, b) : ybs)
  | eq x y = Just b
  | otherwise = lookupBy eq x ybs

-- | Iterate over a list of items and check if the given predicate holds for
--   all of them.
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM = quantM (&&) True

-- | Iterate over a list of items and check if the given predicate holds for
--   at least one of them.
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM = quantM (||) False

-- | Generalization of 'allM' and 'anyM' that is used to implement each of those
--   functions.
quantM :: (Monad m) => (Bool -> Bool -> Bool) -> Bool -> (a -> m Bool) -> [a] -> m Bool
quantM _comp def _p [] = return def
quantM  comp def  p (x : xs) = do
  bx <- p x
  bxs <- quantM comp def p xs
  return $ bx `comp` bxs

-- | Return the 'Left' value. If no 'Left' value is given, an error is raised.
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft: Applied to 'Right'"

-- | Return the 'Right' value. If no 'Right' value is given, an error is raised.
fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight: Applied to 'Left'"
fromRight (Right b) = b
