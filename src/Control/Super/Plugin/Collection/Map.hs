
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides the type for instance dictionaries.
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

newtype Map k a = Map { unMap :: UniqFM (k, a) } deriving Data

instance (Eq k, Eq a) => Eq (Map k a) where
  ma == mb = unMap ma == unMap mb
  ma /= mb = unMap ma /= unMap mb

instance Monoid (Map k a) where
  mempty = empty
  mappend = union
  mconcat = unions

instance (O.Outputable a, O.Outputable k) => O.Outputable (Map k a) where
  ppr = (O.ppr) . unMap

empty :: Map k a
empty = Map $ U.emptyUFM

null :: Map k a -> Bool
null ma = U.isNullUFM $ unMap ma

size :: Map k a -> Int
size ma = U.sizeUFM $ unMap ma

member :: Uniquable k => k -> Map k a -> Bool
member k ma = U.elemUFM k $ unMap ma

notMember :: Uniquable k => k -> Map k a -> Bool
notMember k ma = not $ member k ma

insert :: forall k a. Uniquable k => k -> a -> Map k a -> Map k a
insert k e m = Map $ U.alterUFM (Just . f) (unMap m) k
  where f :: Maybe (k , a) -> (k , a)
        -- Insert a new key and value in the map
        f Nothing = (k, e)
        -- If the key is already present in the map, the associated value is replaced with the supplied value
        f (Just (k', _e)) | getUnique k' == getUnique k = (k', e)
        -- Ignore non matching keys
        f (Just entry) = entry

lookup :: Uniquable k => k -> Map k a -> Maybe a
lookup k m = fmap snd $ U.lookupUFM (unMap m) k

delete :: Uniquable k => k -> Map k a -> Map k a
delete k m = Map $ U.delFromUFM (unMap m) k

union :: Map k a -> Map k a -> Map k a
union ma mb = Map $ U.plusUFM_C (\a _ -> a) (unMap ma) (unMap mb)

unions :: [Map k a] -> Map k a
unions ms = foldl union empty ms

map :: (a -> b) -> Map k a -> Map k b
map f ma = Map $ U.mapUFM (\(k,e) -> (k,f e)) $ unMap ma

filter :: (a -> Bool) -> Map k a -> Map k a
filter p ma = Map $ U.filterUFM (p . snd) $ unMap ma

toList :: Map k a -> [(k, a)]
toList m = U.eltsUFM $ unMap m

fromList :: Uniquable k => [(k, a)] -> Map k a
fromList l = Map $ U.listToUFM $ fmap (\(k, a) -> (k , (k , a))) l

elems :: Map k a -> [a]
elems ma = fmap snd $ toList ma

keysSet :: Uniquable k => Map k a -> S.Set k
keysSet ma = S.fromList $ keys ma

keys :: Map k a -> [k]
keys ma = fmap fst $ toList ma
