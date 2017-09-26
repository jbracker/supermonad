
module Test.Control.Super.Plugin.Collection.Set ( tests, us2s, s2us, eqS ) where

import Data.List ( sort )
import qualified Data.Set as S

import Test.QuickCheck

import qualified Unique as U

import qualified Control.Super.Plugin.Collection.Set as US

import Test.Utils 

tests :: [Test]
tests = 
  [ Test "set_s2us"                        set_s2us
  , Test "set_us2s"                        set_us2s
  , Test "set_member"                      set_member
  , Test "set_notMember"                   set_notMember
  , Test "set_member_existing_elem"        set_member_existing_elem
  , Test "set_notMember_existing_elem"     set_notMember_existing_elem
  , Test "set_null"                        set_null
  , Test "set_size"                        set_size
  , Test "set_empty"                       set_empty
  , Test "set_filter"                      set_filter
  , Test "set_map"                         set_map
  , Test "set_insert"                      set_insert
  , Test "set_delete"                      set_delete
  , Test "set_insert_delete"               set_insert_delete
  , Test "set_delete_insert"               set_delete_insert
  , Test "set_union"                       set_union
  , Test "set_unions"                      set_unions
  , Test "set_intersection"                set_intersection
  , Test "set_difference"                  set_difference
  , Test "set_singleton"                   set_singleton
  , Test "set_isSubsetOf"                  set_isSubsetOf
  , Test "set_isProperSubsetOf"            set_isProperSubsetOf
  ]

us2s :: (Ord a) => US.Set a -> S.Set a
us2s = S.fromList . US.toList

s2us :: (U.Uniquable a) => S.Set a -> US.Set a
s2us = US.fromList . S.toList

-- | Comparing two sets need to make sure that both deliver exactly the same 
--   elements, but the order of the elements is irrelevant.
eqS :: (Ord a) => S.Set a -> US.Set a -> Bool
eqS s us = sort (S.toList s) == sort (US.toList us)

-- | Modify the same map using the standard and custom map implemention and check if the results are the same.
compareModifiedSet :: (U.Uniquable a, Ord a',  Eq a') => (S.Set a -> S.Set a') -> (US.Set a -> US.Set a') -> S.Set a -> Bool
compareModifiedSet sf usf s = eqS (sf s) (usf (s2us s))

compareModifiedSetBin :: (U.Uniquable a, Ord a',  Eq a') => (S.Set a -> S.Set a -> S.Set a') -> (US.Set a -> US.Set a -> US.Set a') -> S.Set a -> S.Set a -> Bool
compareModifiedSetBin sf usf sa sb = eqS (sf sa sb) (usf (s2us sa) (s2us sb)) 

compareSet :: (U.Uniquable a, Eq b) => (S.Set a -> b) -> (US.Set a -> b) -> S.Set a -> Bool
compareSet sf usf s = usf (s2us s) == sf s

compareSetBin :: (U.Uniquable a, U.Uniquable b, Eq c) => (S.Set a -> S.Set b -> c) -> (US.Set a -> US.Set b -> c) -> S.Set a -> S.Set b -> Bool
compareSetBin sf usf sa sb = usf (s2us sa) (s2us sb) == sf sa sb

set_s2us :: S.Set Int -> Bool
set_s2us s = eqS s (s2us s)

set_us2s :: S.Set Int -> Bool
set_us2s s = eqS (us2s (s2us s)) (s2us s)

set_member :: Int -> S.Set Int -> Bool
set_member e = compareSet (S.member e) (US.member e)

set_notMember :: Int -> S.Set Int -> Bool
set_notMember e = compareSet (S.notMember e) (US.notMember e)

set_member_existing_elem :: Int -> S.Set Int -> Property
set_member_existing_elem e s = e `elem` S.toList s ==> compareSet (S.member e) (US.member e) s

set_notMember_existing_elem :: Int -> S.Set Int -> Property
set_notMember_existing_elem e s = e `elem` S.toList s ==> compareSet (S.notMember e) (US.notMember e) s

set_null :: S.Set Int -> Bool
set_null = compareSet (S.null) (US.null)

set_size :: S.Set Int -> Bool
set_size = compareSet (S.size) (US.size)

set_empty :: Bool
set_empty = compareModifiedSet id (const US.empty) (S.empty :: S.Set Int)

set_filter :: Fun Int Bool -> S.Set Int -> Bool
set_filter f = compareModifiedSet (S.filter (applyFun f)) (US.filter (applyFun f))

set_map :: (Fun Int Int) -> S.Set Int -> Bool
set_map f = compareModifiedSet (S.map (applyFun f)) (US.map (applyFun f))

set_insert :: Int -> S.Set Int -> Bool
set_insert e = compareModifiedSet (S.insert e) (US.insert e)

set_delete :: Int -> S.Set Int -> Bool
set_delete e = compareModifiedSet (S.delete e) (US.delete e)

set_insert_delete :: Int -> S.Set Int -> Bool
set_insert_delete e = compareModifiedSet (S.delete e . S.insert e) (US.delete e . US.insert e)

set_delete_insert :: Int -> S.Set Int -> Bool
set_delete_insert e = compareModifiedSet (S.insert e . S.delete e) (US.insert e . US.delete e)

set_union :: S.Set Int -> S.Set Int -> Bool
set_union = compareModifiedSetBin (S.union) (US.union)

set_unions :: [S.Set Int] -> Bool
set_unions ss = eqS (S.unions ss) (US.unions (fmap s2us ss))

set_intersection :: S.Set Int -> S.Set Int -> Bool
set_intersection = compareModifiedSetBin (S.intersection) (US.intersection)

set_difference :: S.Set Int -> S.Set Int -> Bool
set_difference = compareModifiedSetBin (S.difference) (US.difference)

set_singleton :: Int -> Bool
set_singleton i = eqS (S.singleton i) (US.singleton i)

set_isSubsetOf :: S.Set Int -> S.Set Int -> Bool
set_isSubsetOf = compareSetBin (S.isSubsetOf) (US.isSubsetOf)

set_isProperSubsetOf :: S.Set Int -> S.Set Int -> Bool
set_isProperSubsetOf = compareSetBin (S.isProperSubsetOf) (US.isProperSubsetOf)

















