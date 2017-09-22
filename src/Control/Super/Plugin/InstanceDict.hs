
-- | Provides the type for instance dictionaries.
module Control.Super.Plugin.InstanceDict 
  ( InstanceDict
  , emptyInstDict, insertInstDict
  , lookupInstDict, lookupInstDictByTyCon
  , allInstDictTyCons ) where

import Data.Maybe ( maybe )

import Class ( Class )
import InstEnv ( ClsInst )
import TyCon ( TyCon )
import qualified Outputable as O

import qualified Control.Super.Plugin.Collection.Set as S
import qualified Control.Super.Plugin.Collection.Map as M

-- | A dictionary associating supermonad type constructors and classes with 
--   their instances.
--   This is essentially a lookup table for instances associated with a 
--   given type constructor and class.
newtype InstanceDict = InstanceDict (M.Map TyCon (M.Map Class ClsInst))

instance Monoid InstanceDict where
  mappend (InstanceDict a) (InstanceDict b) = InstanceDict $ mappend a b
  mempty = emptyInstDict

instance O.Outputable InstanceDict where
  ppr (InstanceDict instDict) = O.text "InstanceDict " O.<> O.parens (O.ppr instDict)

-- | The empty instance dictionary.
emptyInstDict :: InstanceDict
emptyInstDict = InstanceDict $ M.empty

-- | Insert an entry into a instance dictionary.
insertInstDict :: TyCon -> Class -> ClsInst -> InstanceDict -> InstanceDict
insertInstDict tc cls inst (InstanceDict instDict) 
  = InstanceDict $ M.insert tc (M.insert cls inst (maybe M.empty id $ M.lookup tc instDict)) instDict

-- | Try to lookup an entry in a instance dictionary.
lookupInstDict :: TyCon -> Class -> InstanceDict -> Maybe ClsInst
lookupInstDict tc cls (InstanceDict instDict) = do
  clsDict <- M.lookup tc instDict
  M.lookup cls clsDict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allInstDictTyCons :: InstanceDict -> S.Set TyCon
allInstDictTyCons (InstanceDict instDict) = M.keysSet instDict

-- | Looks up all entries with the given type constructor as key.
--   Returns a mapping between the classes and their instances for that
--   type constructor.
lookupInstDictByTyCon :: TyCon -> InstanceDict -> M.Map Class ClsInst
lookupInstDictByTyCon tc (InstanceDict instDict) = maybe M.empty id $ M.lookup tc instDict


