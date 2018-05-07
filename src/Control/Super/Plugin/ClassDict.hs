
{-# LANGUAGE CPP #-}

-- | Provides the type to store classes and instances used by the plugin.
module Control.Super.Plugin.ClassDict 
  ( ClassDict
  , Optional
  , emptyClsDict
  , insertClsDict, insertOptionalClsDict
  , lookupClsDict
  , isOptionalClass
  , lookupClsDictClass, lookupClsDictInstances
  , allClsDictKeys, allClsDictEntries ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import Data.Semigroup ( Semigroup(..) )
#endif

import Control.Monad ( join )

import Class ( Class )
import InstEnv ( ClsInst(..) )
import qualified Outputable as O

-- | Flag to indicate if a class is optional.
type Optional = Bool

-- | Dictionary type to lookup classes and their available instances based
--   on string identifiers.
newtype ClassDict = ClassDict (M.Map String (Optional, Maybe (Class, [ClsInst])))

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- | Semigroup based on union.
instance Semigroup ClassDict where
  (<>) (ClassDict clsDictA) (ClassDict clsDictB) = ClassDict $ mappend clsDictA clsDictB
#endif

-- | See 'M.union'.
instance Monoid ClassDict where
  mempty = emptyClsDict
  mappend (ClassDict clsDictA) (ClassDict clsDictB) = ClassDict $ mappend clsDictA clsDictB

instance O.Outputable ClassDict where
  ppr (ClassDict clsDict) = O.text "ClassDict " O.<> O.parens (O.ppr clsDict)

-- | The empty class dictionary.
emptyClsDict :: ClassDict
emptyClsDict = ClassDict $ M.empty

-- | Insert an entry into a class dictionary.
insertClsDict :: String -> Optional -> Class -> [ClsInst] -> ClassDict -> ClassDict
insertClsDict key opt cls insts (ClassDict dict) = ClassDict $ M.insert key (opt, Just (cls, insts)) dict

-- | Insert the entry of an optional missing class into the dictionary.
insertOptionalClsDict :: String -> ClassDict -> ClassDict
insertOptionalClsDict key (ClassDict dict) = ClassDict $ M.insert key (True, Nothing) dict

-- | Check if the given class is optional for solving.
isOptionalClass :: String -> ClassDict -> Bool
isOptionalClass key (ClassDict dict) = case M.lookup key dict of
  Nothing       -> False
  Just (opt, _) -> opt

-- | Try to lookup an entry in a class dictionary.
lookupClsDict :: String -> ClassDict -> Maybe (Class, [ClsInst])
lookupClsDict key (ClassDict dict) = join $ fmap snd $ M.lookup key dict

-- | Try to lookup the class in a class dictionary.
lookupClsDictClass :: String -> ClassDict -> Maybe Class
lookupClsDictClass key dict = fmap fst $ lookupClsDict key dict

-- | Try to lookup the 'Control.Supermonad.Applicative' instance of the type constructor.
lookupClsDictInstances :: String -> ClassDict -> Maybe [ClsInst]
lookupClsDictInstances key dict = fmap snd $ lookupClsDict key dict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allClsDictKeys :: ClassDict -> S.Set String
allClsDictKeys (ClassDict dict) = M.keysSet dict

-- | Retrives all of the entries stored in the class dictionary.
allClsDictEntries :: ClassDict -> [(Optional, Maybe (Class, [ClsInst]))]
allClsDictEntries (ClassDict dict) = M.elems dict
