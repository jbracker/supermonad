
{-# LANGUAGE MultiParamTypeClasses  #-} -- for 'Bind' class.
{-# LANGUAGE TypeFamilies #-} -- for 'Bind' class.
{-# LANGUAGE FlexibleContexts #-} -- for 'Bind' class: Allows functor constraint for result

{-# LANGUAGE FlexibleInstances #-} -- for 'Bind Identity a a' and 'Bind a Identity a' instances.

-- | Representation of supermonads in Haskell.
module Control.Supermonad
  ( Bind(..), Return(..), Fail(..)
    -- Reexporting this is convenient for users, because they don't
    -- have to remember to import Data.Functor.Identity separatly anymore.
  , Identity( Identity, runIdentity )
  ) where

import Prelude
  ( String
  , Functor(..)
  , (.), const
  )
import qualified Prelude as P

import Data.Functor.Identity ( Identity( Identity, runIdentity ) )

-- -----------------------------------------------------------------------------
-- Supermonad Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class (Functor m, Functor n, Functor (BindF m n)) => Bind m n where
  type BindF m n :: * -> *
  (>>=) :: m a -> (a -> n b) -> (BindF m n) b
  (>>)  :: m a -> n b -> (BindF m n) b
  ma >> mb = ma >>= const mb

instance (Functor a) => Bind Identity a where
  type BindF Identity a = a
  ma >>= f = f (runIdentity ma)
instance (Functor a) => Bind a Identity where
  type BindF a Identity = a
  ma >>= f = fmap (runIdentity . f) ma

instance Bind [] [] where
  type BindF [] [] = []
  (>>=) = (P.>>=)
instance Bind P.Maybe P.Maybe where
  type BindF P.Maybe P.Maybe = P.Maybe
  (>>=) = (P.>>=)
instance Bind P.IO P.IO where
  type BindF P.IO P.IO = P.IO
  (>>=) = (P.>>=)

-- -----------------------------------------------------------------------------
-- Return Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class Return m where
  return :: a -> m a

instance Return Identity where
  return = P.return
instance Return [] where
  return = P.return
instance Return P.Maybe where
  return = P.return
instance Return P.IO where
  return = P.return

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