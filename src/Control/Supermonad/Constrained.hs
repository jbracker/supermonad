
{-# LANGUAGE MultiParamTypeClasses  #-} -- for 'Bind' class.
{-# LANGUAGE ConstraintKinds        #-} -- for 'Bind' class.
{-# LANGUAGE TypeFamilies           #-} -- for 'Bind' class.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Definition of supermonads that support constrained monads.
module Control.Supermonad.Constrained 
  ( -- * Supermonads
    Bind(..), Return(..), Fail(..)
  , CFunctor(..)
    -- * Conveniences
  , Monad
    -- * Reexports
    -- Reexporting this is convenient for users, because they don't
    -- have to remember to import Data.Functor.Identity separatly anymore.
  , Identity( Identity, runIdentity )
  ) where

import GHC.Exts ( Constraint )

import Prelude
  ( String
  , Ord
  , (.), ($), const
  )
import qualified Prelude as P


-- To define instances:
import Data.Functor.Identity ( Identity(..) )

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

-- To define 'Bind' class:
import Control.Supermonad.Constrained.Functor 
  ( CFunctor(..) )

-- -----------------------------------------------------------------------------
-- Supermonad Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class (CFunctor m, CFunctor n, CFunctor p) => Bind m n p where
  type BindCts m n p (a :: *) (b :: *) :: Constraint
  type BindCts m n p a b = ()
  (>>=) :: (BindCts m n p a b) => m a -> (a -> n b) -> p b
  (>>)  :: (BindCts m n p a b) => m a -> n b -> p b
  ma >> mb = ma >>= const mb

instance Bind Identity Identity Identity where
  (>>=) = (P.>>=)
instance Bind [] [] [] where
  (>>=) = (P.>>=)
instance Bind P.Maybe P.Maybe P.Maybe where
  (>>=) = (P.>>=)
instance Bind P.IO P.IO P.IO where
  (>>=) = (P.>>=)
instance Bind (P.Either e) (P.Either e) (P.Either e) where
  (>>=) = (P.>>=)

instance Bind Mon.First Mon.First Mon.First where
  (>>=) = (P.>>=)
instance Bind Mon.Last Mon.Last Mon.Last where
  (>>=) = (P.>>=)
instance (Bind f f f) => Bind (Mon.Alt f) (Mon.Alt f) (Mon.Alt f) where
  type BindCts (Mon.Alt f) (Mon.Alt f) (Mon.Alt f) a b = BindCts f f f a b
  m >>= f = Mon.Alt $ (Mon.getAlt m) >>= (Mon.getAlt . f)

instance Bind Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (>>=) = (P.>>=)

instance Bind Read.ReadP Read.ReadP Read.ReadP where
  (>>=) = (P.>>=)
instance Bind Read.ReadPrec Read.ReadPrec Read.ReadPrec where
  (>>=) = (P.>>=)

instance Bind (ST.ST s) (ST.ST s) (ST.ST s) where
  (>>=) = (P.>>=)
instance Bind (STL.ST s) (STL.ST s) (STL.ST s) where
  (>>=) = (P.>>=)
instance (Arrow.ArrowApply a) => Bind (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) where
  (>>=) = (P.>>=)
instance (Bind m m m) => Bind (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) where
  type BindCts (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) a b = BindCts m m m a b
  m >>= f = App.WrapMonad $ (App.unwrapMonad m) >>= (App.unwrapMonad . f)

instance Bind STM.STM STM.STM STM.STM where
  (>>=) = (P.>>=)

-- Constrained Instances -------------------------------------------------------

instance Bind S.Set S.Set S.Set where
  type BindCts S.Set S.Set S.Set a b = Ord b
  s >>= f = S.foldr S.union S.empty $ S.map f s

-- -----------------------------------------------------------------------------
-- Return Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class (CFunctor m) => Return m where
  type ReturnCts m (a :: *) :: Constraint
  type ReturnCts m a = ()
  return :: (ReturnCts m a) => a -> m a

instance Return Identity where
  return = P.return
instance Return [] where
  return = P.return
instance Return P.Maybe where
  return = P.return
instance Return P.IO where
  return = P.return
instance Return (P.Either e) where
  return = P.return

instance Return Mon.First where
  return = P.return
instance Return Mon.Last where
  return = P.return
instance (Return m) => Return (Mon.Alt m) where
  type ReturnCts (Mon.Alt m) a = ReturnCts m a
  return a = Mon.Alt $ return a

instance Return Proxy.Proxy where
  return = P.return

instance Return Read.ReadP where
  return = P.return
instance Return Read.ReadPrec where
  return = P.return

instance Return (ST.ST s) where
  return = P.return
instance Return (STL.ST s) where
  return = P.return
instance (Arrow.ArrowApply a) => Return (Arrow.ArrowMonad a) where
  return = P.return
instance (Return m) => Return (App.WrappedMonad m) where
  type ReturnCts (App.WrappedMonad m) a = ReturnCts m a
  return a = App.WrapMonad $ return a

instance Return STM.STM where
  return = P.return

-- Constrained Instances -------------------------------------------------------

instance Return S.Set where
  return = S.singleton

-- -----------------------------------------------------------------------------
-- Fail Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class Fail m where
  fail :: String -> m a

instance Fail Identity where
  fail = P.fail
instance Fail [] where
  fail = P.fail
instance Fail P.Maybe where
  fail = P.fail
instance Fail P.IO where
  fail = P.fail
instance Fail (P.Either e) where
  fail = P.fail

instance Fail Mon.First where
  fail = P.fail
instance Fail Mon.Last where
  fail = P.fail
instance (Fail a) => Fail (Mon.Alt a) where
  fail a = Mon.Alt $ fail a

instance Fail Proxy.Proxy where
  fail = P.fail

instance Fail Read.ReadP where
  fail = P.fail
instance Fail Read.ReadPrec where
  fail = P.fail

instance Fail (ST.ST s) where
  fail = P.fail
instance Fail (STL.ST s) where
  fail = P.fail
instance (Arrow.ArrowApply a) => Fail (Arrow.ArrowMonad a) where
  fail = P.fail
instance (Fail m) => Fail (App.WrappedMonad m) where
  fail a = App.WrapMonad $ fail a

instance Fail STM.STM where
  fail = P.fail

-- Constrained Instances -------------------------------------------------------

instance Fail S.Set where
  fail _ = S.empty

-- -----------------------------------------------------------------------------
-- Convenient type synonyms
-- -----------------------------------------------------------------------------

-- | A short-hand for writing polymorphic standard monad functions.
type family Monad m :: Constraint where
  Monad m = (Bind m m m, Return m, Fail m)



