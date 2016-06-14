
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Supermonad.Applicative
  ( Applicative(..)
  , pure
  ) where

import Prelude
  ( String
  , Maybe(..), Either(..)
  , Functor(..)
  , (.), ($), const, id
  )
import qualified Prelude as P

import Control.Supermonad

-- To define standard instances:
import qualified Data.Monoid as Mon ( First, Last, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )

import qualified Control.Arrow as Arrow ( ArrowMonad, Arrow )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )

-- -----------------------------------------------------------------------------
-- Super-Applicative Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class (Functor m, Functor n, Functor p) => Applicative m n p where
  (<*>) :: m (a -> b) -> n a -> p b
  
  (*>) :: m a -> n b -> p b
  ma *> nb = (id <$ ma) <*> nb
  (<*) :: m a -> n b -> p a
  ma <* nb = fmap const ma <*> nb

-- | 'pure' is defined in terms of return.
pure :: (Return f, ReturnCts f) => a -> f a
pure = return

-- Standard Instances ----------------------------------------------------------

instance Applicative ((->) r) ((->) r) ((->) r) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Identity Identity Identity where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative [] [] [] where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Maybe Maybe Maybe where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative P.IO P.IO P.IO where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative (P.Either e) (P.Either e) (P.Either e) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Mon.First Mon.First Mon.First where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Mon.Last Mon.Last Mon.Last where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance (Applicative m n p) => Applicative (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) where
  -- type BindCts (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) = BindCts m n p
  mf <*> na = Mon.Alt $ (Mon.getAlt mf) <*> (Mon.getAlt na)
  mf *> na = Mon.Alt $ (Mon.getAlt mf) *> (Mon.getAlt na)
  mf <* na = Mon.Alt $ (Mon.getAlt mf) <* (Mon.getAlt na)

instance Applicative Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Read.ReadP Read.ReadP Read.ReadP where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Read.ReadPrec Read.ReadPrec Read.ReadPrec where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative (ST.ST s) (ST.ST s) (ST.ST s) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative (STL.ST s) (STL.ST s) (STL.ST s) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance (Arrow.Arrow a) => Applicative (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

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
instance (Applicative m m m, P.Monad m) => Applicative (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) where
  --type BindCts (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) = BindCts m m m
  mf <*> na = App.WrapMonad $ (App.unwrapMonad mf) <*> (App.unwrapMonad na)
  mf *> na = App.WrapMonad $ (App.unwrapMonad mf) *> (App.unwrapMonad na)
  mf <* na = App.WrapMonad $ (App.unwrapMonad mf) <* (App.unwrapMonad na)

instance Applicative STM.STM STM.STM STM.STM where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)