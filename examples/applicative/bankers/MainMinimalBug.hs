
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

-- Needed for programming on the type level...
{-# LANGUAGE UndecidableInstances #-}

-- Use the supermonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Monad.Plugin #-}

{-# LANGUAGE PartialTypeSignatures #-}

import Control.Super.Monad.Prelude

main :: IO ()
main = return ()

-- Natural numbers on the type level.
data Nat = Z | S Nat

-- Internal function used to compute the maximum of two numbers on the type-level.
type family MaxInternal (nRec :: Nat) (mRec :: Nat) (n :: Nat) (m :: Nat) where
  MaxInternal 'Z mRec n m = m
  MaxInternal ('S nRec) ('Z) n m = n
  MaxInternal ('S nRec) ('S mRec) n m = MaxInternal nRec mRec n m

-- Compute the maximum of two numbers on the type-level.
type family Max (n :: Nat) (m :: Nat) :: Nat where
  Max n m = MaxInternal n m n m

-- Check if n is lesser or equal to m.
type family Leq (n :: Nat) (m :: Nat) :: Bool where
  Leq 'Z m = 'True -- zero is less or equal to anything
  Leq ('S n) 'Z = 'False -- succ of n cannot be smaller then zero.
  Leq ('S n) ('S m) = Leq n m

data MaxRes = MaxRes Nat Nat

data Exec (i :: MaxRes) a = Exec a

instance Functor (Exec i) where
  fmap f (Exec a) = Exec (f a)

instance Return (Exec ('MaxRes 'Z 'Z)) where
  return = Exec

-- This is how it should look:
instance ( Max ma1 ma2 ~ maR, Max mb1 mb2 ~ mbR
         ) => Applicative (Exec ('MaxRes ma1 mb1)) (Exec ('MaxRes ma2 mb2)) (Exec ('MaxRes maR mbR)) where
  Exec f <*> Exec a = Exec (f a)
-- But with this we get problems because the plugin panics during unification.
-- I will have to look into this.
{-
-- The applicative schedules/executes two given processes in parallel.
instance ( 'MaxRes ma1 mb1 ~ m1, 'MaxRes ma2 mb2 ~ m2, 'MaxRes maR mbR ~ mR
         , Max ma1 ma2 ~ maR, Max mb1 mb2 ~ mbR
         ) => Applicative (Exec m1) (Exec m2) (Exec mR) where
  Exec f <*> Exec a = Exec (f a)
-}

type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two
type Four = 'S Three
type Five = 'S Four
type Six = 'S Five

exec1 :: Exec ('MaxRes Three Zero) (a -> a)
exec1 = Exec id

exec2 :: Exec ('MaxRes Five Three) ()
exec2 = Exec ()

-- This is where the unification error emerges.
exec = (exec1 <*> exec2) :: Exec ('MaxRes _ _) _
