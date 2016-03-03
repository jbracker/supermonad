
-- | Functions and utilities to work with and inspect class instances
--   of the GHC API.
module Control.Supermonad.Plugin.Instance
  ( eqInstance
  , instanceClassTyCon
  , instanceTyArgs
  , instanceType
  ) where

import InstEnv
  ( ClsInst(..), IsOrphan(..)
  , instanceSig )
import Type
  ( Type
  , eqTypes )
import Class ( Class, classTyCon )
import TyCon ( TyCon )

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

-- | Returns the type constructors of the class is instance instantiates.
instanceClassTyCon :: ClsInst -> TyCon
instanceClassTyCon inst = classTyCon $ is_cls inst

-- | Returns the arguments of the given instance head.
instanceTyArgs :: ClsInst -> [Type]
instanceTyArgs inst = args
  where (_, _, _, args) = instanceType inst

-- | Returns the class, naming type constructor and arguments of this instance.
instanceType :: ClsInst -> ([Type], Class, TyCon, [Type])
instanceType inst = (cts, cls, instanceClassTyCon inst, args)
  where (_tvs, cts, cls, args) = instanceSig inst
