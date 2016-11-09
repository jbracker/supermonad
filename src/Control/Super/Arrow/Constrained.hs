
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
module Control.Super.Arrow.Constrained
  ( ArrowArr(..)
  , ArrowSequence(..)
  , ArrowSelect(..)
  , ArrowParallel(..)
  , ArrowFanOut(..)
  ) where

import Control.Super.Monad.Constrained.Prelude

--import Control.Monad as M
import qualified Control.Arrow as A

import GHC.Exts ( Constraint )

-- -----------------------------------------------------------------------------
-- Definition of generalized arrows (Superarrows)
-- -----------------------------------------------------------------------------

class ArrowArr f where
  type ArrowArrCts f a b :: Constraint
  type ArrowArrCts f a b = ()
  arr :: (ArrowArrCts f a b) => (a -> b) -> f a b

instance ArrowArr (->) where
  arr = A.arr
instance (Return m) => ArrowArr (A.Kleisli m) where
  type ArrowArrCts (A.Kleisli m) a b = (ReturnCts m b)
  arr f = A.Kleisli $ return . f



infixr 1 <<<
infixr 1 >>>  

class ArrowSequence f g h  where
  type ArrowSequenceCts f g h a b c :: Constraint
  type ArrowSequenceCts f g h a b c = ()
  (<<<) :: (ArrowSequenceCts f g h a b c) => g b c -> f a b -> h a c
  (<<<) = flip (>>>)
  (>>>) :: (ArrowSequenceCts f g h a b c) => f a b -> g b c -> h a c
  (>>>) = flip (<<<)

instance ArrowSequence (->) (->) (->) where
  (>>>) = (A.>>>)
  (<<<) = (A.<<<)
instance (Bind m n p) => ArrowSequence (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowSequenceCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) a b c = (BindCts m n p b c)
  (>>>) (A.Kleisli mf) (A.Kleisli mg) = A.Kleisli $ \a -> mf a >>= \b -> mg b



class ArrowSelect f g where
  type ArrowSelectFstCts f g a b c :: Constraint
  type ArrowSelectFstCts f g a b c = ()
  type ArrowSelectSndCts f g a b c :: Constraint
  type ArrowSelectSndCts f g a b c = ()
  first  :: (ArrowSelectFstCts f g a b c) => f a b -> g (a, c) (b, c)
  second :: (ArrowSelectSndCts f g a b c) => f a b -> g (c, a) (c, b)

instance ArrowSelect (->) (->) where
  first = A.first
  second = A.second

-- FIXME/TODO: Can we do this without using UndecidableInstances?
instance (Bind m n n, Return n) => ArrowSelect (A.Kleisli m) (A.Kleisli n) where
  type ArrowSelectFstCts (A.Kleisli m) (A.Kleisli n) a b c = (BindCts m n n b (b,c), ReturnCts n (b,c))
  type ArrowSelectSndCts (A.Kleisli m) (A.Kleisli n) a b c = (BindCts m n n b (c,b), ReturnCts n (c,b))
  first  (A.Kleisli f) = A.Kleisli (\ ~(a,c) -> f a >>= \b -> return (b,c))
  second (A.Kleisli f) = A.Kleisli (\ ~(c,a) -> f a >>= \b -> return (c,b))
{- This also works:
instance (Bind m m n, Return m) => ArrowSelect (A.Kleisli m) (A.Kleisli n) where
  type ArrowSelectCts (A.Kleisli m) (A.Kleisli n) = (BindCts m m n, ReturnCts m)
  first  (A.Kleisli f) = A.Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (A.Kleisli f) = A.Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
-}


infixr 3 ***
infixr 3 &&&

class ArrowParallel f g h where
  type ArrowParallelCts f g h a b a' b' :: Constraint
  type ArrowParallelCts f g h a b a' b' = ()
  (***) :: (ArrowParallelCts f g h a b a' b') 
        => f a b -> g a' b' -> h (a, a') (b, b')

instance ArrowParallel (->) (->) (->) where
  (***) = (A.***)
instance (Bind m n p, Functor n) => ArrowParallel (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowParallelCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) a b a' b' = (BindCts m n p b (b,b'), FunctorCts n b' (b,b'))
  -- (a -> m b) -> (a' -> n b') -> ((a, a') -> p (b, b'))
  (A.Kleisli f) *** (A.Kleisli g) = A.Kleisli $ \(a, a') -> f a >>= \b -> fmap (\b' -> (b, b')) (g a')



class ArrowFanOut f g h where
  type ArrowFanOutCts f g h a b b' :: Constraint
  type ArrowFanOutCts f g h a b b'= ()
  (&&&) :: (ArrowFanOutCts f g h a b b') => f a b -> g a  b' -> h a (b, b')

instance ArrowFanOut (->) (->) (->) where
  (&&&) = (A.&&&)

instance (Bind m n p, Functor n) => ArrowFanOut (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowFanOutCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) a b b' = (BindCts m n p b (b, b'), FunctorCts n b' (b, b'))
  -- (a -> m b) -> (a  -> n b') -> (a -> p (b, b'))
  (A.Kleisli f) &&& (A.Kleisli g) = A.Kleisli $ \a -> f a >>= \b -> fmap (\b' -> (b, b')) (g a)




