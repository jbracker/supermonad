
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Provides a internal implementation of 'Data.Set.Set's that can be used 
--   with GHC types that are 'Uniquable' but not 'Ord'erable.
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

-- | A set with elements of type @a@.
newtype Set a = Set { unSet :: UniqSet a } deriving Data

-- | Sets can be checked for equality if their elements allow it.
instance (Eq a) => Eq (Set a) where
  sa == sb = unSet sa == unSet sb
  sa /= sb = unSet sa /= unSet sb

-- | Monoid based on union and the empty set.
instance Monoid (Set a) where
  mempty = empty
  mappend = union
  mconcat = unions

instance (O.Outputable a) => O.Outputable (Set a) where
  ppr = (O.ppr) . unSet 

-- | The empty set.
empty :: Set a
empty = Set $ U.emptyUniqSet

-- | Create a set with exactly one element.
singleton :: Uniquable a => a -> Set a
singleton a = Set $ U.unitUniqSet a

-- | Is the set empty?
null :: Set a -> Bool
null s = U.isEmptyUniqSet $ unSet s

-- | Compute the number of elements in the set.
size :: Set a -> Int
size s = U.sizeUniqSet $ unSet s

-- | Check if a given element is in a set.
member :: Uniquable a => a -> Set a -> Bool
member a s = U.elementOfUniqSet a $ unSet s

-- | Check if a given element is not in a set.
notMember :: Uniquable a => a -> Set a -> Bool
notMember a s = not $ member a s

-- | Check if the first set is a subset of the second.
--   The sets may be equal.
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf sub super = intersection sub super == sub

-- | Check if the first set is a subset of the second.
--   The sets may not be equal.
isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf sub super = let i = intersection sub super in i == sub && i /= super

-- | Insert a given element into a set. This will replace any
--   preexisting element with the same unique reprentation in 
--   the set.
insert :: Uniquable a => a -> Set a -> Set a
insert a s = Set $ U.addOneToUniqSet (unSet s) a

-- | Delete a given element from a set. This will remove 
--   any preexisting element with the same unique reprentation in 
--   the set.
delete :: Uniquable a => a -> Set a -> Set a
delete a s = Set $ U.delOneFromUniqSet (unSet s) a

-- | Map the element of a set using the given function.
map :: Uniquable b => (a -> b) -> Set a -> Set b
-- We need to use a custom implementation of "U.mapUniqSet",
-- because back in GHC 7.10 it was implemented wrong.
map f s = Set $ (U.mkUniqSet . fmap f . uniqSetToList) $ unSet s

-- | Filter the elements of a set using the given predicate.
--   Only those elements where the predicate yields 'True' are kept.
filter :: (a -> Bool) -> Set a -> Set a
filter p s = Set $ U.filterUniqSet p $ unSet s

-- | Merge two sets into one with a union.
union :: Set a -> Set a -> Set a
union sa sb = Set $ U.unionUniqSets (unSet sa) (unSet sb)

-- | Merge a list of sets into one using a union.
unions :: [Set a] -> Set a
unions ss = Set $ U.unionManyUniqSets $ fmap unSet ss

-- | Merge two set, but only keep those elements that 
--   were present in both sets.
intersection :: Set a -> Set a -> Set a
intersection sa sb = Set $ U.intersectUniqSets (unSet sa) (unSet sb)

-- | Remove all element that are in the second set from the first one.
difference :: Set a -> Set a -> Set a
difference sa sb = Set $ U.minusUniqSet (unSet sa) (unSet sb)

-- | Remove all element that are in the second set from the first one.
(\\) :: Set a -> Set a -> Set a
(\\) = difference

-- | Convert the set into a list. There is not guarenteed order for the
--   elements to appear in.
toList :: Set a -> [a]
toList s = uniqSetToList $ unSet s

-- | Convert a list into a set. If there are several elements with the 
--   same unique representation only one of them will be kept.
fromList :: Uniquable a => [a] -> Set a
fromList l = Set $ U.mkUniqSet l