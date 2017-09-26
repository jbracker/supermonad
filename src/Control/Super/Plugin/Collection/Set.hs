
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Provides the type for instance dictionaries.
module Control.Super.Plugin.Collection.Set 
  ( Set
  , empty, singleton
  , null, size, member, notMember
  , isSubsetOf, isProperSubsetOf
  , insert, delete
  , filter, map
  , union, unions, intersection, difference, (\\)
  , toList, fromList 
  ) where

import Prelude hiding ( null, filter, map )

import Data.Data ( Data )

import Unique ( Uniquable )
import UniqSet ( UniqSet )
import qualified UniqSet as U
import qualified Outputable as O 
import Control.Super.Plugin.Wrapper ( uniqSetToList )

newtype Set a = Set { unSet :: UniqSet a } deriving Data

instance (Eq a) => Eq (Set a) where
  sa == sb = unSet sa == unSet sb
  sa /= sb = unSet sa /= unSet sb

instance Monoid (Set a) where
  mempty = empty
  mappend = union
  mconcat = unions

instance (O.Outputable a) => O.Outputable (Set a) where
  ppr = (O.ppr) . unSet 

empty :: Set a
empty = Set $ U.emptyUniqSet

singleton :: Uniquable a => a -> Set a
singleton a = Set $ U.unitUniqSet a

null :: Set a -> Bool
null s = U.isEmptyUniqSet $ unSet s

size :: Set a -> Int
size s = U.sizeUniqSet $ unSet s

member :: Uniquable a => a -> Set a -> Bool
member a s = U.elementOfUniqSet a $ unSet s

notMember :: Uniquable a => a -> Set a -> Bool
notMember a s = not $ member a s

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf sub super = intersection sub super == sub

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf sub super = let i = intersection sub super in i == sub && i /= super

insert :: Uniquable a => a -> Set a -> Set a
insert a s = Set $ U.addOneToUniqSet (unSet s) a

delete :: Uniquable a => a -> Set a -> Set a
delete a s = Set $ U.delOneFromUniqSet (unSet s) a

map :: Uniquable b => (a -> b) -> Set a -> Set b
map f s = Set $ U.mapUniqSet f $ unSet s

filter :: (a -> Bool) -> Set a -> Set a
filter p s = Set $ U.filterUniqSet p $ unSet s

union :: Set a -> Set a -> Set a
union sa sb = Set $ U.unionUniqSets (unSet sa) (unSet sb)

unions :: [Set a] -> Set a
unions ss = Set $ U.unionManyUniqSets $ fmap unSet ss

intersection :: Set a -> Set a -> Set a
intersection sa sb = Set $ U.intersectUniqSets (unSet sa) (unSet sb)

difference :: Set a -> Set a -> Set a
difference sa sb = Set $ U.minusUniqSet (unSet sa) (unSet sb)

(\\) :: Set a -> Set a -> Set a
(\\) = difference

toList :: Set a -> [a]
toList s = uniqSetToList $ unSet s

fromList :: Uniquable a => [a] -> Set a
fromList l = Set $ U.mkUniqSet l