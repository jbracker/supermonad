
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

module Main where

import Control.Super.Monad.Prelude

main :: IO ()
main = return ()

-- Natural numbers on the type level.
data Nat = Z | S Nat

-- Internal function used to compute the maximum of two numbers on the 
-- type-level.
type family MaxInternal (nRec :: Nat) (mRec :: Nat) (n :: Nat) (m :: Nat) where
  MaxInternal 'Z _ n m = m
  MaxInternal ('S _) ('Z) n m = n
  MaxInternal ('S nRec) ('S mRec) n m = MaxInternal nRec mRec n m

-- Compute the maximum of two numbers on the type-level.
type family Max (n :: Nat) (m :: Nat) :: Nat where
  Max n m = MaxInternal n m n m

-- Check if n is lesser or equal to m.
type family Leq (n :: Nat) (m :: Nat) :: Bool where
  Leq 'Z _ = 'True -- zero is less or equal to anything
  Leq ('S n) 'Z = 'False -- succ of n cannot be smaller then zero.
  Leq ('S n) ('S m) = Leq n m

-- Counting structure for the process resources and their maximum uses.
data ProcRes = ProcRes Nat Nat Nat Nat

-- Data type to describe processes.
data Proc (i :: ProcRes) (j :: ProcRes) a = Proc a

-- Identifiers for resrouces.
type ResId = Int

-- Open an instance of resource A for use.
-- This entails the open resource counter for A to increase 
-- and the maxiumum use of resource A to be updated.
claimA :: Proc ('ProcRes ma ra mb rb) ('ProcRes (Max ma ('S ra)) ('S ra) mb rb) ResId
claimA = Proc 0

-- Close an instance of resource A.
-- This entails the open resource counter for A to descrease.
releaseA :: ResId -> Proc ('ProcRes ma ('S ra) mb rb) ('ProcRes ma ra mb rb) ()
releaseA _ = Proc ()

-- Open an instance of resource B for use.
-- This entails the open resource counter for B to increase 
-- and the maxiumum use of resource B to be updated.
claimB :: Proc ('ProcRes ma ra mb rb) ('ProcRes ma ra (Max mb ('S rb)) ('S rb)) ResId
claimB = Proc 0

-- Close an instance of resource B.
-- This entails the open resource counter for B to descrease.
releaseB :: ResId -> Proc ('ProcRes ma ra mb ('S rb)) ('ProcRes ma ra mb rb) ()
releaseB _ = Proc ()

-- Functor for processes
instance Functor (Proc i j) where
  fmap f (Proc a) = Proc (f a)
  
-- Return for processes
instance Return (Proc i i) where
  return = Proc

-- Ap for processes
instance Applicative (Proc i j) (Proc j k) (Proc i k) where
  (Proc f) <*> (Proc a) = Proc (f a)

-- Bind for processes
instance Bind (Proc i j) (Proc j k) (Proc i k) where
  (Proc a) >>= f = let Proc b = f a in Proc b

-- Type-level structure to remember the maxiumum use of resource A and B.
data MaxRes = MaxRes Nat Nat

-- Execution applicative which allows to schedule and execute processes
-- that declare their maximum resource use.
data Exec (i :: MaxRes) a = Exec a

-- Create the schedulable and executable version of a process.
-- The process is required not to use any resources in the beginning
-- and to have no open resources in the end. The maximum number of 
-- used resources is transferred to the execution environment.
process :: Proc ('ProcRes 'Z 'Z 'Z 'Z) ('ProcRes ma 'Z mb 'Z) a -> Exec ('MaxRes ma mb) a
process (Proc a) = Exec a

-- Functor for execution
instance Functor (Exec i) where
  fmap f (Exec a) = Exec (f a)

-- Return for execution. Note that this produces an empty execution with 
-- no resource requirements. Thus, the maximum use for A and B are both zero.
-- Since 'Exec' forms a graded monad the index forms the neutral element of the 
-- 'Max' operation on natural numbers.
instance Return (Exec ('MaxRes 'Z 'Z)) where
  return = Exec

-- The applicative schedules/executes two given processes in parallel.
instance ( 'MaxRes ma1 mb1 ~ m1, 'MaxRes ma2 mb2 ~ m2, 'MaxRes maR mbR ~ mR
         , Max ma1 ma2 ~ maR, Max mb1 mb2 ~ mbR
         ) => Applicative (Exec m1) (Exec m2) (Exec mR) where
  Exec f <*> Exec a = Exec (f a)
{-
-- This is how it should look:
instance ( Max ma1 ma2 ~ maR, Max mb1 mb2 ~ mbR
         ) => Applicative (Exec ('MaxRes ma1 mb1)) (Exec ('MaxRes ma2 mb2)) (Exec ('MaxRes maR mbR)) where
  Exec f <*> Exec a = Exec (f a)
-- But with this we get problems because the plugin panics during unification.
-- I will have to look into this.
-}

-- NOTE: We could give a graded monad instance to execute 
-- processes in sequence, but we don't because we want to apply
-- the bankers algorithm to schedule the processes in parallel.
-- This is similar to what Simon Marlow does with applicatives in his query API.

-- Provide a connection between the type-level naturals and the 
-- value-level naturals.
data NatV (n :: Nat) where
  Zero :: NatV 'Z
  Succ :: NatV n -> NatV ('S n)
  
zero = Zero
one = Succ zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five

type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two
type Four = 'S Three
type Five = 'S Four
type Six = 'S Five

-- Implementation of bankers algorithms to schedule the process joined
-- together in execution environment.
-- We need to make sure that the maximim number of available resources 
-- provided to the run functions exceeds the maximum number of 
-- resources required by the processes in the execution environment.
run :: ( Leq ma' ma ~ 'True, Leq mb' mb ~ 'True
       ) => NatV ma -> NatV mb -> Exec ('MaxRes ma' mb') a -> IO a
run ma mb (Exec a) = return a

-- Example process 1
-- Uses resource A 3 times
proc1 = do
  a1 <- claimA
  a2 <- claimA
  a3 <- claimA
  releaseA a3
  releaseA a2
  claimA >>= releaseA -- This sould not change the overall result.
  releaseA a1
  return id

-- Example process 2
-- Uses resource A 5 times and resource B 3 times.
proc2 = do
  a1 <- claimA
  b' <- claimB
  a2 <- claimA
  a3 <- claimA
  releaseB b'
  b1 <- claimB
  b2 <- claimB
  b3 <- claimB
  releaseB b3
  a4 <- claimA
  releaseB b2
  a5 <- claimA
  releaseA a1
  releaseA a2
  releaseB b1
  releaseA a3
  releaseA a4
  releaseA a5
  return ()

exec1 :: Exec ('MaxRes Three Zero) (a -> a)
exec1 = process proc1

exec2 :: Exec ('MaxRes Five Three) ()
exec2 = process proc2

-- Example scheduling of proc1 and proc2 in parallel.
-- Overall this should use a max 5A and 3B
exec = process proc1 <*> process proc2

test = do
  -- This works:
  run five zero exec1
  run five six exec2
  run five three exec
  
  -- This does not work:
  --run zero zero exec
  --run four three exec
  --run five two exec
