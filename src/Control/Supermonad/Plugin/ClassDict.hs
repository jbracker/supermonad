
-- | Provides the type to store classes and instances used by the plugin.
module Control.Supermonad.Plugin.ClassDict 
  ( ClassDict
  , emptyDict, insertDict, lookupDict
  , lookupDictClass, lookupDictInstances
  , allDictKeys ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Class ( Class )
import InstEnv ( ClsInst(..) )

newtype ClassDict = ClassDict (M.Map String (Class, [ClsInst]))

-- | The empty class dictionary.
emptyDict :: ClassDict
emptyDict = ClassDict $ M.empty

-- | Insert an entry into a class dictionary.
insertDict :: String -> Class -> [ClsInst] -> ClassDict -> ClassDict
insertDict key cls insts (ClassDict dict) = ClassDict $ M.insert key (cls, insts) dict

-- | Try to lookup an entry in a class dictionary.
lookupDict :: String -> ClassDict -> Maybe (Class, [ClsInst])
lookupDict key (ClassDict dict) = M.lookup key dict

-- | Try to lookup the class in a class dictionary.
lookupDictClass :: String -> ClassDict -> Maybe Class
lookupDictClass key dict = fmap (\(cls, _) -> cls) $ lookupDict key dict

-- | Try to lookup the 'Control.Supermonad.Applicative' instance of the type constructor.
lookupDictInstances :: String -> ClassDict -> Maybe [ClsInst]
lookupDictInstances key dict = fmap (\(_, insts) -> insts) $ lookupDict key dict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allDictKeys :: ClassDict -> S.Set String
allDictKeys (ClassDict dict) = M.keysSet dict