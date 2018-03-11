
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides a internal implementation of 'Data.Map.Map's that can be used 
--   with GHC types that are 'Uniquable' but not 'Ord'erable.
module Control.Super.Plugin.Collection.Map 
  ( Map
  , empty
  , null, size
  , insert, lookup, delete
  , member, notMember
  , map, filter
  , union, unions
  , fromList, toList
  , elems
  , keysSet, keys
  ) where

import Prelude hiding ( null, lookup, map, filter )

import Data.Data ( Data )

import Unique ( Uniquable, getUnique )
import UniqFM ( UniqFM )
import qualified UniqFM as U
import qualified Outputable as O 

import qualified Control.Super.Plugin.Collection.Set as S

-- | A map with keys of type @k@ and elements of type @a@.
newtype Map k a = Map { unMap :: UniqFM (k, a) } deriving Data

-- | Maps can be checked for equality if their elements and keys allow it.
instance (Eq k, Eq a) => Eq (Map k a) where
  ma == mb = unMap ma == unMap mb
  ma /= mb = unMap ma /= unMap mb

-- | Monoid based on union and the empty map.
instance Monoid (Map k a) where
  mempty = empty
  mappend = union
  mconcat = unions

instance (O.Outputable a, O.Outputable k) => O.Outputable (Map k a) where
  ppr = (O.ppr) . unMap

-- | The empty map.
empty :: Map k a
empty = Map $ U.emptyUFM

-- | Is the map empty?
null :: Map k a -> Bool
null ma = U.isNullUFM $ unMap ma

-- | Count the number of entries in the map.
size :: Map k a -> Int
size ma = U.sizeUFM $ unMap ma

-- | Check if the given key has an entry in the map.
member :: Uniquable k => k -> Map k a -> Bool
member k ma = U.elemUFM k $ unMap ma

-- | Check if the given key does not have an entry in the map.
notMember :: Uniquable k => k -> Map k a -> Bool
notMember k ma = not $ member k ma

-- | Insert the given key value pair in the map. Any preexisting 
--   entry with the same key will be replaced.
insert :: forall k a. Uniquable k => k -> a -> Map k a -> Map k a
insert k e m = Map $ U.alterUFM (Just . f) (unMap m) k
  where f :: Maybe (k , a) -> (k , a)
        -- Insert a new key and value in the map
        f Nothing = (k, e)
        -- If the key is already present in the map, the associated value is replaced with the supplied value
        f (Just (k', _e)) | getUnique k' == getUnique k = (k', e)
        -- Ignore non matching keys
        f (Just entry) = entry

-- | Retrieve the associated entry of the given key, if there is one.
lookup :: Uniquable k => k -> Map k a -> Maybe a
lookup k m = fmap snd $ U.lookupUFM (unMap m) k

-- | Remove the entry with the given key, if it exists.
delete :: Uniquable k => k -> Map k a -> Map k a
delete k m = Map $ U.delFromUFM (unMap m) k

-- | Merge together two maps. If there are two entries with the 
--   same key, the left (first) map will be prefered (left bias).
union :: Map k a -> Map k a -> Map k a
union ma mb = Map $ U.plusUFM_C (\a _ -> a) (unMap ma) (unMap mb)

-- | Merge several maps together. If there are two entries with the 
--   same key, the left-most map in the list will be prefered (left bias).
unions :: [Map k a] -> Map k a
unions ms = foldl union empty ms

-- | Maps the entries of a map using the given function.
map :: (a -> b) -> Map k a -> Map k b
map f ma = Map $ U.mapUFM (\(k,e) -> (k,f e)) $ unMap ma

-- | Filter the value of a map using the given predicate.
--   Only thoes entries that the predicate yields 'True' for
--   will be kept.
filter :: (a -> Bool) -> Map k a -> Map k a
filter p ma = Map $ U.filterUFM (p . snd) $ unMap ma

-- | Convert the map into a list of key value pairs.
toList :: Map k a -> [(k, a)]
toList m = U.eltsUFM $ unMap m

-- | Create a map from a list of key value pairs.
--   If there are several pairs with the same key the entry
--   of the last pair in the list with that key will be kept.
fromList :: Uniquable k => [(k, a)] -> Map k a
fromList l = Map $ U.listToUFM $ fmap (\(k, a) -> (k , (k , a))) l

-- | Create a list of all entries in the map. There is no
--   guarenteed order for the list.
elems :: Map k a -> [a]
elems ma = fmap snd $ toList ma

-- | Create a 'Set' containing all the keys in the map.
keysSet :: Uniquable k => Map k a -> S.Set k
keysSet ma = S.fromList $ keys ma

-- | Create a list containing all the keys in the map.
--   There is no guarenteed order for the list.
keys :: Map k a -> [k]
keys ma = fmap fst $ toList ma
