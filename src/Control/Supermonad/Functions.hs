
-- Needed to specify constraint context that contain 'Identity'.
{-# LANGUAGE FlexibleContexts #-}
-- Needed to use supermonads instead of standard monads.
{-# LANGUAGE RebindableSyntax #-}
-- Needed to use BindF and type equality in constraints.
{-# LANGUAGE TypeFamilies #-}

-- TODO: Remove. For dev purposes
{-# LANGUAGE ScopedTypeVariables #-}

-- Plugin ----------------------------------------------------------------------

{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

-- -----------------------------------------------------------------------------
-- | Collection of the ported monad-based functions for supermonads.
--   For a more detailed description of these functions refer to
--   the 'Control.Monad' module.
--
--   Most functions are generalized to suite the setting of supermonads better.
--
--   This module is thought as a replacement for the "Control.Monad" module.
module Control.Supermonad.Functions
  ( -- * @Control.Monad@ replacements
    -- ** Basic supermonad functions
    mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  , (=<<)
  , (>=>), (<=<)
  , forever, void
    -- ** Generalizations of list functions
  , join
  -- , msum, mfilter -- FIXME: Requires an alternative of 'MonadPlus'.
  -- , filterM -- TODO: Plugin does not support
  , mapAndUnzipM
  , zipWithM, zipWithM_
  , foldM, foldM_
  -- , replicateM, replicateM_
    -- ** Conditional execution of monadic expressions
  -- , guard -- FIXME: Requires an alternative of 'Alternative'
  , when, unless
    -- ** Monadic lifting operators
  , liftM, liftM2, liftM3
  -- , liftM4, liftM5 -- TODO
  , ap
    -- ** Strict monadic functions
  , (<$!>)
    -- * Additional generalized supermonad functions
  , (<$>)
    -- * Addition due to RebindableSyntax
  , ifThenElse
  ) where

import qualified Prelude as P
import Prelude
  ( Bool(..), Int
  , Functor(..)
  , (.), ($)
  , id, flip
  , not
  , fromInteger
  --, const
  --, otherwise
  , (<=), (-) )
--import Data.Foldable ( Foldable(..) )

import Control.Supermonad

infixr 1 =<<
infixr 1 >=>
infixr 1 <=<

-- | Standard implementation of if-then-else. Necessary because we are
--   going to use @RebindableSyntax@ together with this prelude.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _f = t
ifThenElse False _t f = f

-- | Same as '>>=', but with the arguments interchanged.
(=<<) :: (Bind m n) => (a -> n b) -> m a -> BindF m n b
f =<< ma = ma >>= f

-- | Left-to-right Kleisli composition.
(>=>) :: (Bind m n) => (a -> m b) -> (b -> n c) -> a -> BindF m n c
(>=>) f g x = f x >>= g

-- | Right-to-left Kleisli composition.
(<=<) :: (Bind m n) => (b -> n c) -> (a -> m b) -> a -> BindF m n c
(<=<) g f x = f x >>= g

-- | When the condition is true do the given action.
when :: (Functor m, Return m) => Bool -> m () -> m ()
when True  s = void s
when False _ = return ()

-- | When the condition is false do the given action.
unless :: (Functor m, Return m) => Bool -> m () -> m ()
unless b = when (not b)

-- | Ignore the result of a computation.
void :: (Functor m) => m a -> m ()
void = (>> return ())

-- | Map the given function on each element of the list and collect the results.
mapM :: (Return m, Bind m m, m ~ BindF m m) => (a -> m b) -> [a] -> m [b]
mapM f = P.foldr k (return [])
  where
    k a r = do
      x <- f a
      xs <- r
      return (x : xs)

-- | 'mapM' ignoring the result.
mapM_ :: (Return m, Bind m m, m ~ BindF m m) => (a -> m b) -> [a] -> m ()
mapM_ f = void . mapM f

-- | 'flip'ped version of 'mapM'.
forM :: (Return m, Bind m m, m ~ BindF m m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

-- | 'forM' ignoring the result.
forM_ :: (Return m, Bind m m, m ~ BindF m m) => [a] -> (a -> m b) -> m ()
forM_ xs = void . forM xs

-- | Monadic join operation.
join :: (Bind m n) => m (n a) -> BindF m n a
join k = k >>= id

-- | Execute all computations in the list in order and returns the list of results.
sequence :: (Return m, Bind m m, m ~ BindF m m) => [m b] -> m [b]
sequence = mapM id

-- | 'sequence' ignoring the result.
sequence_ :: (Return m, Bind m m, m ~ BindF m m) => [m b] -> m ()
sequence_ = void . sequence

-- | Execute the given computation repeatedly forever.
forever :: (Bind m m, m ~ BindF m m) => m a -> m b
forever na = na >> forever na

-- | Make arguments and result of a pure function monadic.
liftM :: (Functor m) => (a -> b) -> m a -> m b
liftM f ma = ma >>= (return . f)

-- | Make arguments and result of a pure function monadic.
liftM2 :: (Bind m n) --, Bind n Identity n) 
       => (a -> b -> c) -> m a -> n b -> BindF m n c
liftM2 f ma nb = do
  a <- ma
  b <- nb
  return $ f a b

-- | Make arguments and result of a pure function monadic.
liftM3 :: (Bind m (BindF n p), Bind n (BindF p Identity), Bind p Identity)
       => (a -> b -> c -> d) -> m a -> n b -> p c -> BindF m (BindF n p) d
liftM3 f ma nb pc = do --ma >>= (\a -> nb >>= (\b -> pc >>= (\c -> return $ f a b c)))
  a <- ma
  b <- nb
  c <- pc
  return $ f a b c
{- 
-- | Like @filter@ but with a monadic predicate and result.
filterM :: forall m n a. ( Bind n m m --, Bind m m m
           , Return m, Bind m Identity m)
        => (a -> n Bool) -> [a] -> m [a]
filterM _f [] = return []
filterM f (x : xs) = do
  keep <- f x
  if keep
    then filterM f xs >>= (return . (x :))
    else filterM f xs
-- TODO: Can fix by replacing "filterM f xs >>= (return . (x :))" 
-- with "(x :) P.<$> filterM f xs" or adding a type annotation: 
-- "filterM f xs :: m [a]" (ScopedTypeVariables)
-}
-- | Map a given monadic function on the list and the unzip the results.
mapAndUnzipM :: (Return m, Bind m m, m ~ BindF m m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = liftM P.unzip (forM xs f)

-- | Zip together two list using a monadic function.
zipWithM :: (Return m, Bind m m, m ~ BindF m m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ P.zipWith f xs ys

-- | Same as 'zipWithM', but ignores the results.
zipWithM_ :: (Return m, Bind m m, m ~ BindF m m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = void $ zipWithM f xs ys

-- | Fold the given foldable using a monadic function.
--   See 'foldl'.
foldM :: ( P.Foldable t, Return m, Bind m n, m ~ BindF m n) 
      => (b -> a -> n b) -> b -> t a -> m b
foldM f e = P.foldl f' (return e)
  where f' mb a = mb >>= \b -> f b a

-- | Same as 'foldM', but ignores the result.
foldM_ :: (P.Foldable t, Return m, Bind m n, m ~ BindF m n) 
       => (b -> a -> n b) -> b -> t a -> m ()
foldM_ f e = void . foldM f e
{-
-- | Repeats the given monadic operation for the given amount of times and
--   returns the accumulated results.
replicateM :: (Return n, Bind m n n) => Int -> m a -> n [a]
replicateM n _ma | n <= 0 = return []
replicateM n ma = do
  a <- ma
  as <- replicateM (n - 1) ma
  return $ a : as

-- | Same as 'replicateM', but ignores the results.
replicateM_ :: (Return n, Bind m n n) => Int -> m a -> n ()
replicateM_ n = void . replicateM n
-}

-- | Make the resulting function a monadic function.
ap :: (Bind m n) => m (a -> b) -> n a -> BindF m n b
ap mf na = do
  f <- mf
  a <- na
  return $ f a

-- | Apply the given function to the result of a computation.
(<$>) :: (Functor m) => (a -> b) -> m a -> m b
f <$> m = do
  x <- m
  return (f x)

-- | Strict version of '<$>'.
(<$!>) :: (Functor m) => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `P.seq` return z
