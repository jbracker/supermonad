
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE UndecidableInstances #-}

-- Ignore our orphan instance in this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use the polymonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude

import qualified Control.Effect as E
import Control.Effect ( Effect, Plus, Unit, Inv )
import Control.Effect.CounterNat

import GHC.TypeLits

instance Functor (Counter (s :: Nat)) where
  fmap f ma = ma E.>>= (E.return . f)

instance (h ~ (f + g)) => Bind (Counter (f :: Nat)) (Counter (g :: Nat)) (Counter (h :: Nat)) where
  (>>=) = (E.>>=)

instance (h ~ 0) => Return (Counter (h :: Nat)) where
  return = E.return

instance Fail (Counter (h :: Nat)) where
  fail = E.fail

main :: IO ()
main = do
  print $ forget (limitedOp 1 2 3 4)

specialOp :: Int -> Int -> Counter 1 Int
specialOp n m = tick (n + m)

limitedOp :: Int -> Int -> Int -> Int -> Counter 3 Int
limitedOp a b c d = do
  ab <- specialOp a b
  abc <- specialOp ab c
  specialOp abc d
