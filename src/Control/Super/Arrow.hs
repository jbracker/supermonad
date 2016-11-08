
-- Use the polymonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Monad.Plugin #-}

-- for arrow class definitions:
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}

-- for definition of 'ArrowSelect (A.Kleisli m) (A.Kleisli n)' instance:
{-# LANGUAGE UndecidableInstances   #-}

-- | Representation of superarrows in Haskell.
module Control.Super.Arrow
  ( ArrowArr(..)
  , ArrowSequence(..)
  , ArrowSelect(..)
  , ArrowParallel(..)
  , ArrowFanOut(..)
  ) where

import Control.Super.Monad.Prelude hiding ( id, (.) )
import qualified Control.Super.Monad.Prelude as P

--import Control.Monad as M
import qualified Control.Arrow as A

import GHC.Exts ( Constraint )

-- -----------------------------------------------------------------------------
-- Definition of generalized arrows (Superarrows)
-- -----------------------------------------------------------------------------

class ArrowArr f where
  type ArrowArrCts f :: Constraint
  type ArrowArrCts f = ()
  arr :: (ArrowArrCts f) => (b -> c) -> f b c

instance ArrowArr (->) where
  arr = A.arr
instance (Return m) => ArrowArr (A.Kleisli m) where
  type ArrowArrCts (A.Kleisli m) = (ReturnCts m)
  arr f = A.Kleisli $ return P.. f



infixr 1 <<<
infixr 1 >>>  

class ArrowSequence f g h  where
  type ArrowSequenceCts f g h :: Constraint
  type ArrowSequenceCts f g h = ()
  (<<<) :: (ArrowSequenceCts f g h) => g b c -> f a b -> h a c
  (<<<) = flip (>>>)
  (>>>) :: (ArrowSequenceCts f g h) => f a b -> g b c -> h a c
  (>>>) = flip (<<<)

instance ArrowSequence (->) (->) (->) where
  (>>>) = (A.>>>)
  (<<<) = (A.<<<)
instance (Bind m n p) => ArrowSequence (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowSequenceCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) = (BindCts m n p)
  (>>>) (A.Kleisli mf) (A.Kleisli mg) = A.Kleisli $ \a -> mf a >>= \b -> mg b



class ArrowSelect f g where
  type ArrowSelectCts f g :: Constraint
  type ArrowSelectCts f g = ()
  first  :: (ArrowSelectCts f g) => f b c -> g (b, d) (c, d)
  second :: (ArrowSelectCts f g) => f b c -> g (d, b) (d, c)

instance ArrowSelect (->) (->) where
  first = A.first
  second = A.second

-- FIXME/TODO: Can we do this without using UndecidableInstances?
instance (Bind m n n, Return n) => ArrowSelect (A.Kleisli m) (A.Kleisli n) where
  type ArrowSelectCts (A.Kleisli m) (A.Kleisli n) = (BindCts m n n, ReturnCts n)
  first  (A.Kleisli f) = A.Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (A.Kleisli f) = A.Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
{- This also works:
instance (Bind m m n, Return m) => ArrowSelect (A.Kleisli m) (A.Kleisli n) where
  type ArrowSelectCts (A.Kleisli m) (A.Kleisli n) = (BindCts m m n, ReturnCts m)
  first  (A.Kleisli f) = A.Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (A.Kleisli f) = A.Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
-}


infixr 3 ***
infixr 3 &&&

class ArrowParallel f g h where
  type ArrowParallelCts f g h :: Constraint
  type ArrowParallelCts f g h = ()
  (***) :: (ArrowParallelCts f g h) => f b c -> g b' c' -> h (b, b') (c, c')

instance ArrowParallel (->) (->) (->) where
  (***) = (A.***)
instance (Bind m n p, Functor n) => ArrowParallel (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowParallelCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) = (BindCts m n p)
  -- (b -> m c) -> (b' -> n c') -> ((b, b') -> p (c, c'))
  (A.Kleisli f) *** (A.Kleisli g) = A.Kleisli $ \(b, b') -> f b >>= \c -> fmap (\c' -> (c, c')) (g b')



class ArrowFanOut f g h where
  type ArrowFanOutCts f g h :: Constraint
  type ArrowFanOutCts f g h = ()
  (&&&) :: (ArrowFanOutCts f g h) => f b c -> g b  c' -> h b (c, c')

instance ArrowFanOut (->) (->) (->) where
  (&&&) = (A.&&&)

instance (Bind m n p, Functor n) => ArrowFanOut (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowFanOutCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) = (BindCts m n p)
  -- (b -> m c) -> (b  -> n c') -> (b       -> p (c, c'))
  (A.Kleisli f) &&& (A.Kleisli g) = A.Kleisli $ \b -> f b >>= \c -> fmap (\c' -> (c, c')) (g b)




