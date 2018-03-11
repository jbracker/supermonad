
module Test.Control.Super.Plugin.Collection.Map ( tests, um2m, m2um, eqM ) where

import Data.List ( sortBy, sort )
import qualified Data.Map as M

import Test.QuickCheck

import qualified Unique as U

import qualified Control.Super.Plugin.Collection.Map as UM

import Test.Utils
import qualified Test.Control.Super.Plugin.Collection.Set as TUS

tests :: [Test]
tests = 
  [ Test "map_m2um"                        map_m2um
  , Test "map_um2m"                        map_um2m
  , Test "map_lookup_existing_elem"        map_lookup_existing_elem
  , Test "map_lookup_non_existing_elem"    map_lookup_non_existing_elem
  , Test "map_insert"                      map_insert
  , Test "map_delete"                      map_delete
  , Test "map_insert_delete"               map_insert_delete
  , Test "map_delete_insert"               map_delete_insert
  , Test "map_map"                         map_map
  , Test "map_null"                        map_null
  , Test "map_size"                        map_size
  , Test "map_empty"                       map_empty
  , Test "map_member"                      map_member
  , Test "map_notMember"                   map_notMember
  , Test "map_member_existing_elem"        map_member_existing_elem
  , Test "map_notMember_existing_elem"     map_notMember_existing_elem
  , Test "map_union"                       map_union
  , Test "map_unions"                      map_unions
  , Test "map_filter"                      map_filter
  , Test "map_elems"                       map_elems
  , Test "map_keys"                        map_keys
  , Test "map_keysSet"                     map_keysSet
  ]

um2m :: (Ord k) => UM.Map k e -> M.Map k e
um2m = M.fromList . UM.toList

m2um :: (U.Uniquable k) => M.Map k e -> UM.Map k e
m2um = UM.fromList . M.toList

eqM :: (Ord k, Eq e) => M.Map k e -> UM.Map k e -> Bool
eqM m um = sortBy cmp (M.toList m) == sortBy cmp (UM.toList um)
  where cmp :: (Ord k) => (k , e) -> (k , e) -> Ordering
        cmp a b = compare (fst a) (fst b)

-- | Modify the same map using the standard and custom map implemention and check if the results are the same.
compareModifiedMap :: (U.Uniquable k, Ord k',  Eq e') => (M.Map k e -> M.Map k' e') -> (UM.Map k e -> UM.Map k' e') -> M.Map k e -> Bool
compareModifiedMap mf umf m = eqM (mf m) (umf (m2um m))

compareMap :: (U.Uniquable k, Eq a) => (M.Map k e -> a) -> (UM.Map k e -> a) -> M.Map k e -> Bool
compareMap mf umf m = umf (m2um m) == mf m

map_m2um :: M.Map Int String -> Bool
map_m2um m = eqM m (m2um m)

map_um2m :: M.Map Int String -> Bool
map_um2m m = eqM (um2m (m2um m)) (m2um m)

map_lookup_existing_elem :: M.Map Int String -> Bool
map_lookup_existing_elem m = all (\(k,e) -> UM.lookup k (m2um m) == Just e) (M.toList m)

map_lookup_non_existing_elem :: M.Map Int String -> Int -> Property
map_lookup_non_existing_elem m k = 
  k `notElem` M.keys m 
    ==> UM.lookup k (UM.fromList (M.toList m)) == Nothing

map_insert :: Int -> String -> M.Map Int String -> Bool
map_insert k e = compareModifiedMap (M.insert k e) (UM.insert k e)

map_delete :: Int -> M.Map Int String -> Bool
map_delete k = compareModifiedMap (M.delete k) (UM.delete k)

map_insert_delete :: Int -> String -> M.Map Int String -> Bool
map_insert_delete k e = compareModifiedMap (M.delete k . M.insert k e) (UM.delete k . UM.insert k e)

map_delete_insert :: Int -> String -> M.Map Int String -> Bool
map_delete_insert k e = compareModifiedMap (M.insert k e . M.delete k) (UM.insert k e . UM.delete k)

map_map :: (Fun String [Int]) -> M.Map Int String -> Bool
map_map f = compareModifiedMap (M.map (applyFun f)) (UM.map (applyFun f))

map_null :: M.Map Int String -> Bool
map_null = compareMap (M.null) (UM.null)

map_size :: M.Map Int String -> Bool
map_size = compareMap (M.size) (UM.size)

map_empty :: Bool
map_empty = compareModifiedMap id (const UM.empty) (M.empty :: M.Map Int String)

map_member :: Int -> M.Map Int String -> Bool
map_member e = compareMap (M.member e) (UM.member e)

map_notMember :: Int -> M.Map Int String -> Bool
map_notMember e = compareMap (M.notMember e) (UM.notMember e)

map_member_existing_elem :: Int -> M.Map Int String -> Property
map_member_existing_elem e m = e `elem` M.keys m ==> compareMap (M.member e) (UM.member e) m

map_notMember_existing_elem :: Int -> M.Map Int String -> Property
map_notMember_existing_elem e m = e `elem` M.keys m ==> compareMap (M.notMember e) (UM.notMember e) m

map_union :: M.Map Int String -> M.Map Int String -> Bool
map_union ma mb = eqM (M.union ma mb) (UM.union (m2um ma) (m2um mb))

map_unions :: [M.Map Int String] -> Bool
map_unions ms = eqM (M.unions ms) (UM.unions (fmap m2um ms))

map_filter :: Fun String Bool -> M.Map Int String -> Bool
map_filter f = compareModifiedMap (M.filter (applyFun f)) (UM.filter (applyFun f))

map_elems :: M.Map Int String -> Bool
-- We have to compare sorted lists just in case the order of the 
-- produced lists differs between implementations.
map_elems m = sort (M.elems m) == sort (UM.elems (m2um m))

map_keys :: M.Map Int String -> Bool
-- We have to compare sorted lists just in case the order of the 
-- produced lists differs between implementations.
map_keys m = sort (M.keys m) == sort (UM.keys (m2um m))

map_keysSet :: M.Map Int Int -> Bool
map_keysSet m = TUS.eqS (M.keysSet m) (UM.keysSet (m2um m))


















