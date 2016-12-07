
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RankNTypes #-}

-- Use the supermonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Arrow.Plugin #-}

module Main 
  ( Count(..)
  , MaxF
  , carr
  , main
  ) where

import GHC.TypeLits

import Data.Proxy ( Proxy(..) )

import Control.Super.Arrow.Prelude
import Control.Super.Arrow

main :: IO ()
main = return ()

newtype Count arr (i :: Nat) a b = Count (arr a b)

-- Sadly this requires 'UndecidableInstances'...
type family MaxF (i :: Nat) (j :: Nat) :: Nat where
  MaxF 0 j = j
  MaxF i 0 = i
  MaxF i i = i
  MaxF i j = MaxF (i - 1) (j - 1)

{-
instance (Category arr) => Category (Count arr i) where
  id = arr id
  (Count f) . (Count g) = Count $ f . g
-}

carr :: (ArrowArr arr, ArrowArrCts arr) => Proxy n -> (a -> b) -> Count arr n a b
carr _ f = Count $ arr f 

instance ArrowArr arr => ArrowArr (Count arr 0) where
  type ArrowArrCts (Count f 0) = ArrowArrCts f
  arr = Count . arr

instance (ArrowSequence f g h, k ~ (i + j)) => ArrowSequence (Count f i) (Count g j) (Count h k)  where
  type ArrowSequenceCts (Count f i) (Count g j) (Count h k) = ArrowSequenceCts f g h
  (Count f) >>> (Count g) = Count $ f >>> g

instance (ArrowSelect f g, i ~ j) => ArrowSelect (Count f i) (Count g j) where
  type ArrowSelectCts (Count f i) (Count g j) = ArrowSelectCts f g
  first  (Count f) = Count $ first f 
  second (Count f) = Count $ second f

instance (ArrowParallel f g h, k ~ MaxF i j) => ArrowParallel (Count f i) (Count g j) (Count h k) where
  type ArrowParallelCts (Count f i) (Count g j) (Count h k) = ArrowParallelCts f g h
  (Count f) *** (Count g) = Count $ f *** g

instance (ArrowFanOut f g h, k ~ MaxF i j) => ArrowFanOut (Count f i) (Count g j) (Count h k) where
  type ArrowFanOutCts (Count f i) (Count g j) (Count h k) = ArrowFanOutCts f g h
  (Count f) &&& (Count g) = Count $ f &&& g









