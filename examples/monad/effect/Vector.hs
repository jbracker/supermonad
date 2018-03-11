
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- Ignore our orphan instance in this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use the supermonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude hiding ( concatMap, map )

import qualified Control.Effect as E
import Control.Effect ( Plus, Inv )

data Nat = Z | S Nat

data Vector (n :: Nat) a where
  Nil :: Vector Z a
  Cons :: a -> Vector n a -> Vector (S n) a

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add Z     m = m
  Add (S n) m = S (Add n m)

type family Mult (n :: Nat) (m :: Nat) :: Nat where 
  Mult Z     m = Z
  Mult (S n) m = Add m (Mult n m)

map :: (a -> b) -> Vector n a -> Vector n b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

concatMap :: Vector n a -> (a -> Vector m b) -> Vector (Mult n m) b
concatMap Nil         f = Nil
concatMap (Cons x xs) f = append (f x) (concatMap xs f)

singleton :: a -> Vector (S Z) a
singleton a = Cons a Nil

instance E.Effect Vector where
  type Unit Vector = S Z
  type Plus Vector n m = Mult n m
  type Inv Vector n m = ()
  
  return = singleton
  (>>=) = concatMap

instance Functor (Vector (n :: Nat)) where
  fmap f xs = map f xs

instance ( nm ~ Plus Vector n m
         ) => Bind (Vector (n :: Nat)) (Vector (m :: Nat)) (Vector (nm :: Nat)) where
  type BindCts (Vector (n :: Nat)) (Vector (m :: Nat)) (Vector (nm :: Nat)) = Inv Vector n m
  (>>=) = (E.>>=)

instance Return (Vector (S Z)) where
  return = E.return

instance ( nm ~ Plus Vector n m
         ) => Applicative (Vector (n :: Nat)) (Vector (m :: Nat)) (Vector (nm :: Nat)) where
  type ApplicativeCts (Vector (n:: Nat)) (Vector (m :: Nat)) (Vector (nm :: Nat)) = Inv Vector n m
  mf <*> ma = mf E.>>= \f -> fmap f ma

instance Fail (Vector (n :: Nat)) where
  fail = E.fail
  
main :: IO ()
main = return ()