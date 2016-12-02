
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Super.Monad.Alternative
  ( AlternativeEmpty(..)
  , AlternativeAlt(..)
  ) where

import qualified Prelude as P
import qualified Control.Applicative as A

import GHC.Exts ( Constraint )

import qualified GHC.Generics as Generics
import qualified GHC.Conc as STM
import qualified Control.Arrow as Arrow
import qualified Control.Applicative as Applic
import qualified Data.Semigroup as Semigroup
import qualified Data.Proxy as Proxy
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Control.Super.Monad.Prelude 
  ( ($)
  , Return(..), Applicative(..) )


class Return f => AlternativeEmpty f where
  type AlternativeEmptyCts f :: Constraint
  type AlternativeEmptyCts f = ()
  empty :: AlternativeEmptyCts f => f a

instance AlternativeEmpty [] where
  empty = A.empty
instance AlternativeEmpty P.Maybe where
  empty = A.empty
instance AlternativeEmpty P.IO where
  empty = A.empty
instance AlternativeEmpty ReadP.ReadP where
  empty = A.empty
instance AlternativeEmpty ReadPrec.ReadPrec where
  empty = A.empty
instance AlternativeEmpty STM.STM where
  empty = A.empty
instance AlternativeEmpty Semigroup.Option where
  empty = A.empty
instance AlternativeEmpty Proxy.Proxy where
  empty = A.empty

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance AlternativeEmpty Generics.U1 where
  empty = A.empty
instance AlternativeEmpty f => AlternativeEmpty (Generics.Rec1 f) where
  type AlternativeEmptyCts (Generics.Rec1 f) = AlternativeEmptyCts f
  empty = Generics.Rec1 empty
  


class Applicative f g h => AlternativeAlt f g h where
  type AlternativeAltCts f g h :: Constraint
  type AlternativeAltCts f g h = ()
  (<|>) :: AlternativeAltCts f g h => f a -> g a -> h a

instance AlternativeAlt [] [] [] where
  (<|>) = (A.<|>)
instance AlternativeAlt P.Maybe P.Maybe P.Maybe where
  (<|>) = (A.<|>)
instance AlternativeAlt P.IO P.IO P.IO where
  (<|>) = (A.<|>)
instance AlternativeAlt ReadP.ReadP ReadP.ReadP ReadP.ReadP where
  (<|>) = (A.<|>)
instance AlternativeAlt ReadPrec.ReadPrec ReadPrec.ReadPrec ReadPrec.ReadPrec where
  (<|>) = (A.<|>)
instance AlternativeAlt STM.STM STM.STM STM.STM where
  (<|>) = (A.<|>)
instance AlternativeAlt Semigroup.Option Semigroup.Option Semigroup.Option where
  (<|>) = (A.<|>)
instance AlternativeAlt Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (<|>) = (A.<|>)

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance AlternativeAlt Generics.U1 Generics.U1 Generics.U1 where
  (<|>) = (A.<|>)
instance AlternativeAlt f g h => AlternativeAlt (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) where
  type AlternativeAltCts (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) = AlternativeAltCts f g h
  (Generics.Rec1 f) <|> (Generics.Rec1 g) = Generics.Rec1 $ f <|> g























