
-- | Provides all kinds of functions that are needed by the plugin.
module Control.Supermonad.Plugin.Utils (
    errIndent
  -- * Type inspection
  , collectTopTyCons
  , collectTopTcVars
  , collectTopTcVarsWithArity
  , collectTyVars
  , mkTcVarSubst
  -- * General Utilities
  , skolemVarsBindFun
  , eqTyVar, eqTyVar'
  , getTyConName
  , isAmbiguousType
  , partiallyApplyTyCons
  , applyTyCon
  , splitKindFunOfTcTv
  , atIndex
  , t1st, t2nd, t3rd
  , associations
  , subsets
  , removeDup
  , lookupBy
  , allM, anyM
  , fromLeft, fromRight
  , partitionM
  ) where

import Data.Maybe ( listToMaybe, catMaybes )
import Data.List ( find )
import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad ( forM )
import Control.Arrow ( second )

import BasicTypes ( Arity )
import Name ( nameOccName )
import OccName ( occNameString )
import Type
  ( Type, TyVar
  , getTyVar_maybe
  , tyConAppTyCon_maybe
  , splitTyConApp_maybe, splitFunTy_maybe, splitAppTy_maybe
  , getEqPredTys_maybe
  , splitAppTys
  , mkTyConTy, mkTyVarTy, mkAppTys
  , eqType )
import TyCon 
  ( TyCon
  , tyConKind, tyConName )
import Var ( tyVarKind )
import TcType ( isAmbiguousTyVar )
import Kind ( Kind )
import Unify ( BindFlag(..) )
import InstEnv ( instanceBindFun )
import TcPluginM ( TcPluginM, newFlexiTyVar )
import Outputable ( ($$) )
import qualified Outputable as O

import Control.Supermonad.Plugin.Wrapper 
  ( TypeVarSubst
  , mkTypeVarSubst
  , splitKindFunTys
  , fromLeft, fromRight
  )

-- -----------------------------------------------------------------------------
-- Constants
-- -----------------------------------------------------------------------------

-- | Indentation to be used in error messages.
errIndent :: Int
errIndent = 4

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
collectTopTcVars = S.map fst . collectTopTcVarsWithArity

-- | Retrieve the type constructor variables at the top level involved in the
--   given types. If there are nested type variables they are ignored.
--   There is no actual check if the returned type variables are actually type
--   constructor variables. Also associates the appearant arity of the given
--   type variables by looking at how many arguments it was applied to.
--
--   /Example/:
--
-- >>> collectTopTcVars [m a b, Identity c, n]
-- { (m, 2), (n, 0) }
collectTopTcVarsWithArity :: [Type] -> Set (TyVar, Arity)
collectTopTcVarsWithArity tys = S.fromList $ catMaybes $ fmap getTyVarAndArity tys
  where
    getTyVarAndArity :: Type -> Maybe (TyVar, Arity)
    getTyVarAndArity t = do
      let (tf, _args) = splitAppTys t
      tv <- getTyVar_maybe tf
      return (tv, tyVarArity tv)

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
mkTcVarSubst :: [(TyVar, TyCon)] -> TypeVarSubst
mkTcVarSubst substs = mkTypeVarSubst $ fmap (second mkTyConTy) substs

-- -----------------------------------------------------------------------------
-- General utilities
-- -----------------------------------------------------------------------------

-- | Override the standard bind flag of a given list of variables to 'Skolem'.
--   The standard bind flag is determined using 'instanceBindFun'.
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

-- | Returns the arity of a given type variable.
tyVarArity :: TyVar -> Arity
tyVarArity = length . fst . splitKindFunTys . tyVarKind

-- | Returns the string representation of the given type constructor in the source code.
getTyConName :: TyCon -> String
getTyConName = occNameString . nameOccName . tyConName

-- | Get the element of a list at a given index (If that element exists).
atIndex :: [a] -> Int -> Maybe a
atIndex xs i = listToMaybe $ drop i xs

t1st :: (a, b, c) -> a
t1st (a, _, _) = a

t2nd :: (a, b, c) -> b
t2nd (_, b, _) = b

t3rd :: (a, b, c) -> c
t3rd (_, _, c) = c

-- | Checks if the given type is an ambiguous type variable.
isAmbiguousType :: Type -> Bool
isAmbiguousType ty = maybe False isAmbiguousTyVar $ getTyVar_maybe ty

-- | Takes a list of type variables that are associated with certain type 
--   constructors or type constructor variables and partially applies them
--   to match the kind of the type variable. Example:
--   
-- >>> partiallyApplyTyCons [(n :: *, Left (Maybe :: * -> *))]
-- Right [(n :: *, Maybe i :: *, [i :: *])]
-- 
-- >>> partiallyApplyTyCons [(n :: * -> *, Right (k :: * -> Int -> * -> *))]
-- Right [(n :: * -> *, k i j :: * -> *, [i :: *, j :: Int])]
-- 
-- >>> partiallyApplyTyCons [(n :: * -> *, Left (Int :: *)]
-- Left ...
-- 
-- >>> partiallyApplyTyCons [(n :: * -> *, Left (Maybe :: * -> *)]
-- Right [(n :: * -> *, Maybe :: * -> *, [])]
-- 
-- The variables generated for the partial application are flexi vars (see 'newFlexiTyVar' and 'applyTyCon').
partiallyApplyTyCons :: [(TyVar, Either TyCon TyVar)] -> TcPluginM (Either O.SDoc [(TyVar, Type, [TyVar])])
partiallyApplyTyCons [] = return $ Right []
partiallyApplyTyCons ((tv, tc) : assocs) = do
    let (tvKindArgs, tvKindRes) = splitKindFunOfTcTv $ Right tv
    let (tcKindArgs, tcKindRes) = splitKindFunOfTcTv tc
    
    let checkKindLength = length tcKindArgs >= length tvKindArgs
    let checkKindMatch = and (uncurry eqType <$> zip (reverse tvKindArgs) (reverse tcKindArgs)) && eqType tcKindRes tvKindRes
    case (checkKindLength, checkKindMatch) of
      (False, _) -> return $ Left $ O.text "Kind mismatch between type constructor and type variable: " 
                                 $$ O.ppr tcKindArgs $$ O.ppr tvKindArgs
      (_, False) -> return $ Left $ O.text "Kind mismatch between type constructor and type variable: " 
                                 $$ O.ppr tc $$ O.ppr tv
      _ -> do 
        eAppliedAssocs <- partiallyApplyTyCons assocs
        case eAppliedAssocs of
          Left err -> return $ Left err
          Right appliedAssocs -> do
            -- Apply as many new type variables to the type constructor as are 
            -- necessary for its kind to match that of the type variable.
            (appliedTcTy, argVars) <- applyTyCon (tc, take (length tcKindArgs - length tvKindArgs) tcKindArgs)
            return $ Right $ (tv, appliedTcTy, argVars) : appliedAssocs

-- | Applies the given type constructor or type constructor variable to 
--   new correctly kinded variables to make it a (partially) applied type. 
--   The (partially) applied type is returned together with the variables 
--   that were applied to the type constructor.
applyTyCon :: (Either TyCon TyVar, [Kind]) -> TcPluginM (Type, [TyVar])
applyTyCon (eTcTv, ks) = do
  tyVarArgs <- forM ks newFlexiTyVar
  let t = either mkTyConTy mkTyVarTy eTcTv
  return $ (mkAppTys t $ fmap mkTyVarTy tyVarArgs, tyVarArgs)

-- | Retrieves the kind of the given type constructor or variables
--   and splits it into its arguments and result. If the kind is not 
--   a function kind then the arguments will be empty.
splitKindFunOfTcTv :: Either TyCon TyVar -> ([Kind], Kind)
splitKindFunOfTcTv tc = case tc of 
  Left tyCon -> splitKindFunTys $ tyConKind tyCon
  Right tyVar -> splitKindFunTys $ tyVarKind tyVar

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

-- | Partition a list into two lists based on a predicate involving a monadic 
--   computation.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x : xs) = do
  (ts, fs) <- partitionM p xs
  b <- p x
  return $ if b then (x : ts, fs) else (ts, x : fs)
  