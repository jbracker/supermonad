
-- Use the polymonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

-- for arrow class definitions:
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}

-- for definition of 'ArrowSelect (A.Kleisli m) (A.Kleisli n)' instance:
{-# LANGUAGE UndecidableInstances   #-}

-- | Representation of superarrows in Haskell.
module Control.Superarrow
  ( CategoryId(..)
  , CategoryCompose(..)
  , (Control.Superarrow.>>>), (Control.Superarrow.<<<)
  , ArrowArr(..)
  , ArrowSelect(..)
  , ArrowCombine(..)
  ) where

import Data.Type.Equality ( (:~:)(Refl) )
import Data.Type.Coercion ( Coercion(Coercion) )

import Control.Supermonad.Prelude hiding ( id, (.) )
import qualified Control.Supermonad.Prelude as P

--import Control.Monad as M
import Control.Arrow as A

import GHC.Exts ( Constraint, coerce )

-- -----------------------------------------------------------------------------
-- Definition of generalized categories
-- -----------------------------------------------------------------------------

infixr 9 .
infixr 1 <<<
infixr 1 >>>

class CategoryId cat where
  type CategoryIdCts cat :: Constraint
  type CategoryIdCts cat = ()
  id :: (CategoryIdCts cat) => cat a a

class CategoryCompose ca cb cc where
  type CategoryComposeCts ca cb cc :: Constraint
  type CategoryComposeCts ca cb cc = ()
  (.) :: (CategoryComposeCts ca cb cc) => ca b c -> cb a b -> cc a c

(<<<) :: ( CategoryCompose ca cb cc, CategoryComposeCts ca cb cc
         ) => ca b c -> cb a b -> cc a c
(<<<) = (.)

(>>>) :: ( CategoryCompose ca cb cc, CategoryComposeCts ca cb cc
         ) => cb a b -> ca b c -> cc a c
(>>>) = flip (.)

instance CategoryId (->) where
  id x = x
instance (Return m) => CategoryId (A.Kleisli m) where
  type CategoryIdCts (A.Kleisli m) = (ReturnCts m)
  id = A.Kleisli return
instance CategoryId (:~:) where
  id = Refl
instance CategoryId Coercion where
  id = Coercion

instance CategoryCompose (->) (->) (->) where
  (f . g) x = f (g x)
instance (Bind n m p) => CategoryCompose (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type CategoryComposeCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) = (BindCts n m p)
  (A.Kleisli f) . (A.Kleisli g) = A.Kleisli (\b -> g b >>= f)
instance CategoryCompose (:~:) (:~:) (:~:) where
  Refl . Refl = Refl
instance CategoryCompose Coercion Coercion Coercion where
  (.) Coercion = coerce

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

class ArrowCombine f g h where
  type ArrowCombineCts f g h :: Constraint
  type ArrowCombineCts f g h = ()
  (***) :: (ArrowCombineCts f g h) => f b c -> g b' c' -> h (b, b') (c, c')
  (&&&) :: (ArrowCombineCts f g h) => f b c -> g b  c' -> h b (c, c')

instance ArrowCombine (->) (->) (->) where
  (***) = (A.***)
  (&&&) = (A.&&&)
instance (Bind m n p, Functor n) => ArrowCombine (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) where
  type ArrowCombineCts (A.Kleisli m) (A.Kleisli n) (A.Kleisli p) = (BindCts m n p)
  -- (b -> m c) -> (b' -> n c') -> ((b, b') -> p (c, c'))
  (A.Kleisli f) *** (A.Kleisli g) = A.Kleisli $ \(b, b') -> f b >>= \c -> fmap (\c' -> (c, c')) (g b')
  -- (b -> m c) -> (b  -> n c') -> (b       -> p (c, c'))
  (A.Kleisli f) &&& (A.Kleisli g) = A.Kleisli $ \b -> f b >>= \c -> fmap (\c' -> (c, c')) (g b)




