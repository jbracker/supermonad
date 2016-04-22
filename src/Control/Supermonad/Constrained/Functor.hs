
{-# LANGUAGE ConstraintKinds #-} -- 'CFunctor' class.
{-# LANGUAGE TypeFamilies    #-} -- 'CFunctor' class.

module Control.Supermonad.Constrained.Functor 
  ( CFunctor(..)
  ) where

import Prelude
  ( Ord
  , (.), ($), const
  )

import GHC.Exts ( Constraint )

import qualified Prelude as P

-- To define instances:
import Data.Functor.Identity ( Identity )

import qualified Data.Monoid as Mon ( First, Last, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )

-- To defined constrained instances:
import qualified Data.Set as S

-- -----------------------------------------------------------------------------
-- Constrained functor class
-- -----------------------------------------------------------------------------

-- | Class for constrained functors. Obeys all of the same laws as the standard
--   'Functor' class, but allows to constrain the functors result type.
class CFunctor f where
  type CFunctorCts (f :: * -> *) (a :: *) (b :: *) :: Constraint
  type CFunctorCts f a b = ()
  
  fmap :: (CFunctorCts f a b) => (a -> b) -> f a -> f b
  (<$) :: (CFunctorCts f b a) => a -> f b -> f a
  (<$) = fmap . const

-- Unconstrained instances -----------------------------------------------------

instance CFunctor Identity where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor [] where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor P.Maybe where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor P.IO where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor (P.Either e) where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor Mon.First where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor Mon.Last where
  fmap = P.fmap
  (<$) = (P.<$)
instance (CFunctor f) => CFunctor (Mon.Alt f) where
  type CFunctorCts (Mon.Alt f) a b = CFunctorCts f a b
  fmap f (Mon.Alt ma) = Mon.Alt $ fmap f ma
  a <$ (Mon.Alt mb) = Mon.Alt $ a <$ mb

instance CFunctor Proxy.Proxy where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor Read.ReadP where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor Read.ReadPrec where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor (ST.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor (STL.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (Arrow.ArrowApply a) => CFunctor (Arrow.ArrowMonad a) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (CFunctor m) => CFunctor (App.WrappedMonad m) where
  type CFunctorCts (App.WrappedMonad m) a b = CFunctorCts m a b
  fmap f (App.WrapMonad ma) = App.WrapMonad $ fmap f ma
  a <$ (App.WrapMonad mb) = App.WrapMonad $ a <$ mb

instance CFunctor STM.STM where
  fmap = P.fmap
  (<$) = (P.<$)

-- Constrained instances -------------------------------------------------------

instance CFunctor S.Set where
  type CFunctorCts S.Set a b = Ord b
  fmap = S.map