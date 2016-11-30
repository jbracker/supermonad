
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Super.Monad.Constrained.Alternative
  ( AlternativeEmpty(..)
  , AlternativeAlt(..)
  ) where

import qualified Prelude as P

import GHC.Exts ( Constraint )

import Control.Super.Monad.Constrained.Prelude ( Return(..), Applicative(..) )

class Return f => AlternativeEmpty f where
  type AlternativeEmptyCts f a :: Constraint
  type AlternativeEmptyCts f a = ()
  empty :: AlternativeEmptyCts f a => f a

class Applicative f g h => AlternativeAlt f g h where
  type AlternativeAltCts f g h a :: Constraint
  type AlternativeAltCts f g h a = ()
  (<|>) :: AlternativeAltCts f g h a => f a -> g a -> h a