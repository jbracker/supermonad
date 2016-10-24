
-- | Provides the type for instance dictionaries.
module Control.Supermonad.Plugin.InstanceDict 
  ( InstanceDict
  , emptyInstDict, insertInstDict, lookupInstDict
  , allTyConsInstDict ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Class ( Class )
import InstEnv ( ClsInst )
import TyCon ( TyCon )

-- | A dictionary associating supermonad type constructors and classes with 
--   their instances.
--   This is essentially a lookup table for instances associated with a 
--   given type constructor and class.
newtype InstanceDict = InstanceDict (M.Map (TyCon, Class) ClsInst)

instance Monoid InstanceDict where
  mappend (InstanceDict a) (InstanceDict b) = InstanceDict $ mappend a b
  mempty = emptyInstDict

-- | The empty instance dictionary.
emptyInstDict :: InstanceDict
emptyInstDict = InstanceDict $ M.empty

-- | Insert an entry into a instance dictionary.
insertInstDict :: TyCon -> Class -> ClsInst -> InstanceDict -> InstanceDict
insertInstDict tc cls inst (InstanceDict instDict) 
  = InstanceDict $ M.insert (tc, cls) inst instDict

-- | Try to lookup an entry in a instance dictionary.
lookupInstDict :: TyCon -> Class -> InstanceDict -> Maybe ClsInst
lookupInstDict tc cls (InstanceDict instDict) = M.lookup (tc, cls) instDict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allTyConsInstDict :: InstanceDict -> S.Set TyCon
allTyConsInstDict (InstanceDict instDict) = S.map fst $ M.keysSet instDict




