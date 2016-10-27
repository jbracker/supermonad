
-- | Provides the type for instance dictionaries.
module Control.Supermonad.Plugin.InstanceDict 
  ( InstanceDict
  , emptyInstDict, insertInstDict
  , lookupInstDict, lookupInstDictByTyCon
  , allInstDictTyCons ) where

import Data.Maybe ( fromJust )
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Class ( Class )
import InstEnv ( ClsInst )
import TyCon ( TyCon )
import qualified Outputable as O

-- | A dictionary associating supermonad type constructors and classes with 
--   their instances.
--   This is essentially a lookup table for instances associated with a 
--   given type constructor and class.
newtype InstanceDict = InstanceDict (M.Map (TyCon, Class) ClsInst)

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
  = InstanceDict $ M.insert (tc, cls) inst instDict

-- | Try to lookup an entry in a instance dictionary.
lookupInstDict :: TyCon -> Class -> InstanceDict -> Maybe ClsInst
lookupInstDict tc cls (InstanceDict instDict) = M.lookup (tc, cls) instDict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allInstDictTyCons :: InstanceDict -> S.Set TyCon
allInstDictTyCons (InstanceDict instDict) = S.map fst $ M.keysSet instDict

-- | Looks up all entries with the given type constructor as key.
--   Returns a mapping between the classes and their instances for that
--   type constructor.
lookupInstDictByTyCon :: TyCon -> InstanceDict -> M.Map Class ClsInst
lookupInstDictByTyCon tc (InstanceDict instDict) 
  -- The use of fromJust is safe here, because the keys we look up were
  -- taken from the existing entries of the map we are looking into.
  = foldr (\cls m -> M.insert cls (fromJust $ M.lookup (tc, cls) instDict) m) M.empty clsKeys
  where
    clsKeys = fmap snd $ filter ((tc ==) . fst) $ M.keys instDict


