
-- | Provides the type to store classes and instances used by the plugin.
module Control.Supermonad.Plugin.ClassDict 
  ( ClassDict
  , emptyClsDict, insertClsDict, lookupClsDict
  , lookupClsDictClass, lookupClsDictInstances
  , allClsDictKeys, allClsDictEntries ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Class ( Class )
import InstEnv ( ClsInst(..) )

newtype ClassDict = ClassDict (M.Map String (Class, [ClsInst]))

-- | The empty class dictionary.
emptyClsDict :: ClassDict
emptyClsDict = ClassDict $ M.empty

-- | Insert an entry into a class dictionary.
insertClsDict :: String -> Class -> [ClsInst] -> ClassDict -> ClassDict
insertClsDict key cls insts (ClassDict dict) = ClassDict $ M.insert key (cls, insts) dict

-- | Try to lookup an entry in a class dictionary.
lookupClsDict :: String -> ClassDict -> Maybe (Class, [ClsInst])
lookupClsDict key (ClassDict dict) = M.lookup key dict

-- | Try to lookup the class in a class dictionary.
lookupClsDictClass :: String -> ClassDict -> Maybe Class
lookupClsDictClass key dict = fmap (\(cls, _) -> cls) $ lookupClsDict key dict

-- | Try to lookup the 'Control.Supermonad.Applicative' instance of the type constructor.
lookupClsDictInstances :: String -> ClassDict -> Maybe [ClsInst]
lookupClsDictInstances key dict = fmap (\(_, insts) -> insts) $ lookupClsDict key dict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allClsDictKeys :: ClassDict -> S.Set String
allClsDictKeys (ClassDict dict) = M.keysSet dict

-- | Retrives all of the entries stored in the class dictionary.
allClsDictEntries :: ClassDict -> [(Class, [ClsInst])]
allClsDictEntries (ClassDict dict) = M.elems dict