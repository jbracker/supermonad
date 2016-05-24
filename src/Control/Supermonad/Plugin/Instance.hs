
-- | Functions and utilities to work with and inspect class instances
--   of the GHC API.
module Control.Supermonad.Plugin.Instance
  ( --eqInstance
    instanceClass
  , instanceClassTyCon
  , instanceTyArgs
  , isClassInstance
  , isMonoTyConInstance
  , isPolyTyConInstance
  ) where

import qualified Data.Set as S

import InstEnv
  ( ClsInst(..) --, IsOrphan(..)
  , instanceHead )
import Type
  ( Type
  , eqTypes )
import Class ( Class, classTyCon )
import TyCon ( TyCon )

import Control.Supermonad.Plugin.Utils ( collectTopTyCons )
{-
-- | Check as best as possible if two class instances are equal.
eqInstance :: ClsInst -> ClsInst -> Bool
eqInstance instA instB
  =  is_cls_nm instA == is_cls_nm instB
  && is_tcs instA == is_tcs instB
  && is_tvs instA == is_tvs instB
  && is_cls instA == is_cls instB
  && length (is_tys instA) == length (is_tys instB)
  && eqTypes (is_tys instA) (is_tys instB)
  && is_dfun instA == is_dfun instB
  && is_flag instA == is_flag instB
  && eqOrphan (is_orphan instA) (is_orphan instB)

-- | Check equality of two 'IsOrphan' values.
eqOrphan :: IsOrphan -> IsOrphan -> Bool
eqOrphan IsOrphan IsOrphan = True
eqOrphan (NotOrphan nameA) (NotOrphan nameB) = nameA == nameB
eqOrphan _ _ = False
-}
-- | Checks if the given instance is of the given type class.
isClassInstance :: Class -> ClsInst -> Bool
isClassInstance cls inst = instanceClass inst == cls

-- | Returns the type class of the given instance.
instanceClass :: ClsInst -> Class
instanceClass = is_cls

-- | Returns the type constructors of the class is instance instantiates.
instanceClassTyCon :: ClsInst -> TyCon
instanceClassTyCon inst = classTyCon $ instanceClass inst

-- | Returns the arguments of the given instance head.
instanceTyArgs :: ClsInst -> [Type]
instanceTyArgs inst = args
  where (_, _, args) = instanceHead inst

-- | Check if the given instance has the following head 
--   @C (M ...) ... (M ...)@ where @M@ is the given type
--   constructor and @C@ is the given class. The arguments of the @M@s
--   do not have to be equal to each other.
isMonoTyConInstance :: TyCon -> Class -> ClsInst -> Bool
isMonoTyConInstance tc cls inst
  =  isClassInstance cls inst 
  && all (== S.singleton tc) argTopTcs
  where
    argTopTcs :: [S.Set TyCon]
    argTopTcs = fmap ( collectTopTyCons . (: []) ) $ instanceTyArgs inst

-- | Checks if the given instance is from the given class, but does not form 
--   a mono type constructor instance as in 'isMonoTyConInstance'.
isPolyTyConInstance :: Class -> ClsInst -> Bool
isPolyTyConInstance cls inst = isClassInstance cls inst && allNotEmpty && not (allEqual argTopTcs)
  where
    argTopTcs :: [S.Set TyCon]
    argTopTcs = fmap ( collectTopTyCons . (: []) ) $ instanceTyArgs inst
    
    allNotEmpty = all (not . S.null) argTopTcs
    
    allEqual [] = True
    allEqual (a:as) = all (a ==) as






