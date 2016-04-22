
-- Needed to specify constraint context that contain 'Identity'.
{-# LANGUAGE FlexibleContexts #-}
-- Needed to use supermonads instead of standard monads.
{-# LANGUAGE RebindableSyntax #-}

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
module Control.Supermonad.Constrained.Functions
  ( -- * @Control.Monad@ replacements
    -- ** Basic supermonad functions
    mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  , (=<<)
  , (>=>), (<=<)
  , forever, void, void'
    -- ** Generalizations of list functions
  , join
  -- , msum, mfilter -- FIXME: Requires an alternative of 'MonadPlus'.
  , filterM -- FIXME / TODO: Plugin does not support without type annotation
  , mapAndUnzipM
  , zipWithM, zipWithM_
  , foldM, foldM_
  , replicateM, replicateM_ -- FIXME / TODO: Plugin does not support without type annotation
    -- ** Conditional execution of monadic expressions
  -- , guard -- FIXME: Requires an alternative of 'Alternative'
  , when, unless
    -- ** Monadic lifting operators
  , liftM, liftM', liftM2, liftM3
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
  , (.), ($)
  , id, flip, const
  , not
  , fromInteger
  --, const
  --, otherwise
  , (<=), (-) )
--import Data.Foldable ( Foldable(..) )

import Control.Supermonad.Constrained

infixr 1 =<<
infixr 1 >=>
infixr 1 <=<

-- | Standard implementation of if-then-else. Necessary because we are
--   going to use @RebindableSyntax@ together with this prelude.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _f = t
ifThenElse False _t f = f

-- | Same as '>>=', but with the arguments interchanged.
(=<<) :: (Bind m n p, BindCts m n p a b) => (a -> n b) -> m a -> p b
f =<< ma = ma >>= f

-- | Left-to-right Kleisli composition.
(>=>) :: (Bind m n p, BindCts m n p b c) => (a -> m b) -> (b -> n c) -> a -> p c
(>=>) f g x = f x >>= g

-- | Right-to-left Kleisli composition.
(<=<) :: (Bind m n p, BindCts m n p b c) => (b -> n c) -> (a -> m b) -> a -> p c
(<=<) g f x = f x >>= g

-- | When the condition is true do the given action.
when :: (Return n, Bind m n n, BindCts m n n () ()) => Bool -> m () -> n ()
when True  s = void' s
when False _ = return ()

-- | When the condition is false do the given action.
unless :: (Return n, Bind m n n, BindCts m n n () ()) => Bool -> m () -> n ()
unless b = when (not b)

-- | Map the given function on each element of the list and collect the results.
mapM :: ( Return n, Bind m n n, BindCts m n n b [b]
        , CFunctorCts n [b] [b]
        ) => (a -> m b) -> [a] -> n [b]
mapM f = P.foldr k (return [])
  where
    k a r = do
      x <- f a
      fmap (x :) r

-- | 'mapM' ignoring the result.
mapM_ :: ( Return n, Bind m n n, BindCts m n n b [b]
         , CFunctorCts n [b] (), CFunctorCts n [b] [b]
         ) => (a -> m b) -> [a] -> n ()
mapM_ f = void . mapM f

-- | 'flip'ped version of 'mapM'.
forM :: ( Return n, Bind m n n, BindCts m n n b [b]
        , CFunctorCts n [b] [b]
        ) => [a] -> (a -> m b) -> n [b]
forM = flip mapM

-- | 'forM' ignoring the result.
forM_ :: ( Return n, Bind m n n, BindCts m n n b [b]
         , CFunctorCts n [b] (), CFunctorCts n [b] [b]
         ) => [a] -> (a -> m b) -> n ()
forM_ xs = void . forM xs

-- | Monadic join operation.
join :: (Bind m n p, BindCts m n p (n a) a) => m (n a) -> p a
join k = k >>= id

-- | Ignore the result of a computation.
void :: (CFunctor m, CFunctorCts m a ()) => m a -> m ()
void = fmap (const ())

-- | Ignore the result of a computation, but allow morphing the computational type.
void' :: ( Return n, Bind m n n, BindCts m n n a ()
         ) => m a -> n ()
void' = (>> return ())

-- | Execute all computations in the list in order and returns the list of results.
sequence :: (Return n, Bind m n n, BindCts m n n b [b], CFunctorCts n [b] [b]) => [m b] -> n [b]
sequence = mapM id

-- | 'sequence' ignoring the result.
sequence_ :: ( Return n, Bind m n n, BindCts m n n b [b]
             , CFunctorCts n [b] (), CFunctorCts n [b] [b]
             ) => [m b] -> n ()
sequence_ = void . sequence

-- | Execute the given computation repeatedly forever.
forever :: (Bind m n n, BindCts m n n a b) => m a -> n b
forever na = na >> forever na

-- | Like @filter@ but with a monadic predicate and result.
filterM :: ( Bind m n n, BindCts m n n Bool [a]
           , Return n
           , CFunctorCts n [a] [a])
        => (a -> m Bool) -> [a] -> n [a]
filterM _f [] = return []
filterM f (x : xs) = do
  keep <- f x
  if keep
    then fmap (x :) $ filterM f xs
    else filterM f xs

-- | Map a given monadic function on the list and the unzip the results.
mapAndUnzipM :: ( Return n, Bind m n n, BindCts m n n (b, c) [(b, c)]
                , CFunctorCts n [(b, c)] ([b], [c]), CFunctorCts n [(b, c)] [(b, c)]
                ) => (a -> m (b, c)) -> [a] -> n ([b], [c])
mapAndUnzipM f xs = liftM P.unzip (forM xs f)

-- | Zip together two list using a monadic function.
zipWithM :: ( Return n, Bind m n n, BindCts m n n c [c]
            , CFunctorCts n [c] [c]
            ) => (a -> b -> m c) -> [a] -> [b] -> n [c]
zipWithM f xs ys = sequence $ P.zipWith f xs ys

-- | Same as 'zipWithM', but ignores the results.
zipWithM_ :: ( Return n, Bind m n n, BindCts m n n c [c]
             , CFunctorCts n [c] (), CFunctorCts n [c] [c]
             ) => (a -> b -> m c) -> [a] -> [b] -> n ()
zipWithM_ f xs ys = void $ zipWithM f xs ys

-- | Fold the given foldable using a monadic function.
--   See 'foldl'.
foldM :: ( P.Foldable t, Return m, Bind m n m, BindCts m n m b b
         ) => (b -> a -> n b) -> b -> t a -> m b
foldM f e = P.foldl f' (return e)
  where f' nb a = nb >>= \b -> f b a

-- | Same as 'foldM', but ignores the result.
foldM_ :: ( P.Foldable t, Return m, Bind m n m, BindCts m n m b b
          , CFunctorCts m b ()
          ) => (b -> a -> n b) -> b -> t a -> m ()
foldM_ f e = void . foldM f e

-- | Repeats the given monadic operation for the given amount of times and
--   returns the accumulated results.
replicateM :: ( Return n, Bind m n n, BindCts m n n a [a]
              , CFunctorCts n [a] [a]
              ) => Int -> m a -> n [a]
replicateM n _ma | n <= 0 = return []
replicateM n ma = do
  a <- ma
  fmap (a :) $ replicateM (n - 1) ma
  -- Rewrite for less restrictive constraints.
  --return $ a : as

-- | Same as 'replicateM', but ignores the results.
replicateM_ :: ( Return n, Bind m n n, BindCts m n n a [a]
               , CFunctorCts n [a] (), CFunctorCts n [a] [a]
               ) => Int -> m a -> n ()
replicateM_ n = void . replicateM n

-- | Make arguments and result of a pure function monadic.
liftM :: (CFunctor m, CFunctorCts m a b) => (a -> b) -> m a -> m b
liftM f ma = fmap f ma

-- | Make arguments and result of a pure function monadic with allowed morphing
liftM' :: (Return n, Bind m n n, BindCts m n n a b) => (a -> b) -> m a -> n b
liftM' f ma = ma >>= (return . f)

-- | Make arguments and result of a pure function monadic.
liftM2 :: (Bind m n p, BindCts m n p a c
          , CFunctorCts n b c
          ) => (a -> b -> c) -> m a -> n b -> p c
liftM2 f ma nb = do
  a <- ma 
  fmap (f a) nb
  -- Rewritten because the constraint are simpler this way.
  -- (Bind m p p, n p p, Return p)
  {- do
  a <- ma
  b <- nb
  return $ f a b
  -}

-- | Make arguments and result of a pure function monadic.
liftM3 :: ( Bind m q q, BindCts m q q a d
          , Bind n p q, BindCts n p q b d
          , CFunctorCts p c d)
       => (a -> b -> c -> d) -> m a -> n b -> p c -> q d
liftM3 f ma nb pc = do --ma >>= (\a -> nb >>= (\b -> pc >>= (\c -> return $ f a b c)))
  a <- ma
  b <- nb
  fmap (f a b) pc
  --return $ f a b c

-- | Make the resulting function a monadic function.
ap :: ( Bind m n p, BindCts m n p (a -> b) b
      , CFunctorCts n a b
      ) => m (a -> b) -> n a -> p b
ap mf na = do
  f <- mf
  fmap f na
  -- Remove the necessity of a 'Return' constraint.
  --return $ f a

-- | Apply the given function to the result of a computation.
(<$>) :: (Return n, Bind m n n, BindCts m n n a b) => (a -> b) -> m a -> n b
f <$> m = do
  x <- m
  return (f x)

-- | Strict version of '<$>'.
(<$!>) :: (Return n, Bind m n n, BindCts m n n a b) => (a -> b) -> m a -> n b
f <$!> m = do
  x <- m
  let z = f x
  z `P.seq` return z
