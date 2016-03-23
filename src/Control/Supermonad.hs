
{-# LANGUAGE MultiParamTypeClasses  #-} -- for 'Bind' class.
{-# LANGUAGE FunctionalDependencies #-} -- for 'Bind' class.

{-# LANGUAGE FlexibleInstances #-} -- for 'Bind Identity a a' and 'Bind a Identity a' instances.

{-# LANGUAGE NoImplicitPrelude #-}

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
  , (.), ($), const
  )
import qualified Prelude as P

import Data.Functor.Identity ( Identity( Identity, runIdentity ) )


-- To define instances:
import qualified Data.Monoid as Mon ( First, Last, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )


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
-- | This instance is valid. Together with the instances for 'P.Maybe' and list
--   it still forms a supermonad.
instance Bind P.Maybe [] [] where
  (P.Just a) >>= f = f a
  P.Nothing  >>= f = []
instance Bind P.IO P.IO P.IO where
  (>>=) = (P.>>=)
instance Bind (P.Either e) (P.Either e) (P.Either e) where
  (>>=) = (P.>>=)

instance Bind Mon.First Mon.First Mon.First where
  (>>=) = (P.>>=)
instance Bind Mon.Last Mon.Last Mon.Last where
  (>>=) = (P.>>=)
instance (Bind f f f) => Bind (Mon.Alt f) (Mon.Alt f) (Mon.Alt f) where
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
-- | TODO / FIXME: The wrapped monad instances for 'Functor' and 'P.Monad' are both 
--   based on @m@ being a monad, although the functor instance should only be 
--   dependend on @m@ being a functor (as can be seen below). This can only be
--   fixed by either giving a custom version of 'App.WrappedMonad' here or by fixing
--   the version of 'App.WrappedMonad' in base.
--   Once this issue is fixed we can replace the 'P.Monad' constraint
--   with a 'Functor' constraint.
--   
--   > instance (Functor m) => Functor (App.WrappedMonad m) where
--   >   fmap f m = App.WrapMonad $ fmap (App.unwrapMonad m) f
--   
instance (Bind m m m, P.Monad m) => Bind (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) where
  m >>= f = App.WrapMonad $ (App.unwrapMonad m) >>= (App.unwrapMonad . f)

instance Bind STM.STM STM.STM STM.STM where
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
instance Return (P.Either e) where
  return = P.return

instance Return Mon.First where
  return = P.return
instance Return Mon.Last where
  return = P.return
instance (Return a) => Return (Mon.Alt a) where
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
-- | TODO / FIXME: This has the same issue as the 'Bind' instance for 'App.WrappedMonad'.
instance (Return m, P.Monad m) => Return (App.WrappedMonad m) where
  return a = App.WrapMonad $ return a

instance Return STM.STM where
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
-- | TODO / FIXME: This has the same issue as the 'Bind' instance for 'App.WrappedMonad'.
instance (Fail m, P.Monad m) => Fail (App.WrappedMonad m) where
  fail a = App.WrapMonad $ fail a

instance Fail STM.STM where
  fail = P.fail