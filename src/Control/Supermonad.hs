
{-# LANGUAGE MultiParamTypeClasses  #-} -- for 'Bind' class.
{-# LANGUAGE FunctionalDependencies #-} -- for 'Bind' class.

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
class (Functor m, Functor n, Functor p) => Bind m n p | m n -> p where
  (>>=) :: m a -> (a -> n b) -> p b
  (>>)  :: m a -> n b -> p b
  ma >> mb = ma >>= const mb

instance (Functor a) => Bind Identity a a where
  ma >>= f = f (runIdentity ma)
instance (Functor a) => Bind a Identity a where
  ma >>= f = fmap (runIdentity . f) ma

instance Bind [] [] [] where
  (>>=) = (P.>>=)
instance Bind P.Maybe P.Maybe P.Maybe where
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
  fail _ = P.Nothing