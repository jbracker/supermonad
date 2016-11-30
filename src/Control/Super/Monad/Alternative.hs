
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Super.Monad.Alternative
  ( AlternativeEmpty(..)
  , AlternativeAlt(..)
  ) where

import qualified Prelude as P

import GHC.Exts ( Constraint )

import Control.Super.Monad.Prelude ( Return(..), Applicative(..) )


class Return f => AlternativeEmpty f where
  type AlternativeEmptyCts f :: Constraint
  type AlternativeEmptyCts f = ()
  empty :: AlternativeEmptyCts f => f a

class Applicative f g h => AlternativeAlt f g h where
  type AlternativeAltCts f g h :: Constraint
  type AlternativeAltCts f g h = ()
  (<|>) :: AlternativeAltCts f g h => f a -> g a -> h a