
-- | Functions and utilities to work with and inspect class instances
--   of the GHC API.
module Control.Super.Plugin.Instance
  ( instanceClass
  , instanceClassTyCon
  , instanceTopTyCons
  , instanceTyArgs
  , isClassInstance
  , isMonoTyConInstance
  , isPolyTyConInstance
  ) where

import InstEnv
  ( ClsInst(..)
  , instanceHead )
import Type ( Type )
import Class ( Class, classTyCon )
import TyCon ( TyCon )

import qualified Control.Super.Plugin.Collection.Set as S
import Control.Super.Plugin.Utils ( collectTopTyCons )

-- | Checks if the given instance is of the given type class.
isClassInstance :: Class -> ClsInst -> Bool
isClassInstance cls inst = instanceClass inst == cls

-- | Returns the type class of the given instance.
instanceClass :: ClsInst -> Class
instanceClass = is_cls

-- | Returns the type constructors of the class is instance instantiates.
instanceClassTyCon :: ClsInst -> TyCon
instanceClassTyCon inst = classTyCon $ instanceClass inst

-- | Collects the top type constructors of the instance arguments.
instanceTopTyCons :: ClsInst -> S.Set TyCon
instanceTopTyCons = collectTopTyCons . instanceTyArgs

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






