
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Supermonad.Applicative
  ( Applicative(..)
  , pure
  ) where

import Prelude hiding ( Monad(..), Applicative(..) )

import Control.Supermonad

pure :: (Return f, ReturnCts f) => a -> f a
pure = return

class (Functor m, Functor n, Functor p) => Applicative m n p where
  (<*>) :: m (a -> b) -> n a -> p b
  
  (*>) :: m a -> n b -> p b
  ma *> nb = (id <$ ma) <*> nb
  (<*) :: m a -> n b -> p a
  ma <* nb = fmap const ma <*> nb


