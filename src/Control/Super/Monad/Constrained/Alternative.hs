
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-} -- For ':.:' instance.

{-# LANGUAGE TypeOperators #-} -- For ':*:' instance and others.

module Control.Super.Monad.Constrained.Alternative
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
import qualified Data.Proxy as Proxy
import qualified Data.Monoid as Mon
import qualified Data.Functor.Product as Product ( Product(..) )
import qualified Data.Functor.Compose as Compose ( Compose(..) )
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import qualified Data.Semigroup as Semigroup
#endif

import Control.Super.Monad.Constrained.Prelude 
  ( ($)
  , Return(..), Applicative(..), Functor(..) )

class Return f => AlternativeEmpty f where
  type AlternativeEmptyCts f a :: Constraint
  type AlternativeEmptyCts f a = ()
  empty :: AlternativeEmptyCts f a => f a

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
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeEmpty Semigroup.Option where
  empty = A.empty
#endif
instance AlternativeEmpty Proxy.Proxy where
  empty = A.empty
instance (AlternativeEmpty f) => AlternativeEmpty (Mon.Alt f) where
  type AlternativeEmptyCts (Mon.Alt f) a = AlternativeEmptyCts f a
  empty = Mon.Alt $ empty

instance (AlternativeEmpty f, AlternativeEmpty f') => AlternativeEmpty (Product.Product f f') where
  type AlternativeEmptyCts (Product.Product f f') a = (AlternativeEmptyCts f a, AlternativeEmptyCts f' a)
  empty = Product.Pair empty empty

instance (AlternativeEmpty f, AlternativeEmpty f') => AlternativeEmpty (Compose.Compose f f') where
  type AlternativeEmptyCts (Compose.Compose f f') a = (AlternativeEmptyCts f (f' a))
  empty = Compose.Compose $ empty

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance AlternativeEmpty Generics.U1 where
  empty = A.empty
instance AlternativeEmpty f => AlternativeEmpty (Generics.Rec1 f) where
  type AlternativeEmptyCts (Generics.Rec1 f) a = AlternativeEmptyCts f a
  empty = Generics.Rec1 empty
instance (AlternativeEmpty f, AlternativeEmpty g) => AlternativeEmpty (f Generics.:*: g) where
  type AlternativeEmptyCts (f Generics.:*: g) a = (AlternativeEmptyCts f a, AlternativeEmptyCts g a)
  empty = empty Generics.:*: empty
instance (AlternativeEmpty f, AlternativeEmpty g) => AlternativeEmpty (f Generics.:.: g) where
  type AlternativeEmptyCts (f Generics.:.: g) a = (AlternativeEmptyCts f (g a))
  empty = Generics.Comp1 $ empty
instance AlternativeEmpty f => AlternativeEmpty (Generics.M1 i c f) where
  type AlternativeEmptyCts (Generics.M1 i c f) a = AlternativeEmptyCts f a
  empty = Generics.M1 $ empty
  

class Applicative f g h => AlternativeAlt f g h where
  type AlternativeAltCts f g h a :: Constraint
  type AlternativeAltCts f g h a = ()
  (<|>) :: AlternativeAltCts f g h a => f a -> g a -> h a

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
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeAlt Semigroup.Option Semigroup.Option Semigroup.Option where
  (<|>) = (A.<|>)
#endif
instance AlternativeAlt Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (<|>) = (A.<|>)
instance (AlternativeAlt f g h) => AlternativeAlt (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) where
  type AlternativeAltCts (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) a = AlternativeAltCts f g h a
  (Mon.Alt ma) <|> (Mon.Alt na) = Mon.Alt $ ma <|> na

instance (AlternativeAlt f g h, AlternativeAlt f' g' h') => AlternativeAlt (Product.Product f f') (Product.Product g g') (Product.Product h h') where
  type AlternativeAltCts (Product.Product f f') (Product.Product g g') (Product.Product h h') a = (AlternativeAltCts f g h a, AlternativeAltCts f' g' h' a)
  Product.Pair m1 m2 <|> Product.Pair n1 n2 = Product.Pair (m1 <|> n1) (m2 <|> n2)

-- TODO: This does the application of '<|>' on the inner type constructors, whereas the original 
-- implementation for the standard classes applies '<|>' on the outer type constructors.
instance (Applicative f g h, AlternativeAlt f' g' h') => AlternativeAlt (Compose.Compose f f') (Compose.Compose g g') (Compose.Compose h h') where
  type AlternativeAltCts (Compose.Compose f f') (Compose.Compose g g') (Compose.Compose h h') a = ( ApplicativeCts f g h (g' a) (h' a), AlternativeAltCts f' g' h' a
                                                                                                  , FunctorCts f (f' a) (g' a -> h' a) )
  (Compose.Compose f) <|> (Compose.Compose g) = Compose.Compose $ fmap (<|>) f <*> g 

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance AlternativeAlt Generics.U1 Generics.U1 Generics.U1 where
  (<|>) = (A.<|>)
instance AlternativeAlt f g h => AlternativeAlt (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) where
  type AlternativeAltCts (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) a = AlternativeAltCts f g h a
  (Generics.Rec1 f) <|> (Generics.Rec1 g) = Generics.Rec1 $ f <|> g
instance (AlternativeAlt f g h, AlternativeAlt f' g' h') => AlternativeAlt (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') where
  type AlternativeAltCts (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') a = (AlternativeAltCts f g h a, AlternativeAltCts f' g' h' a)
  (f Generics.:*: g) <|> (f' Generics.:*: g') = (f <|> f') Generics.:*: (g <|> g')
-- TODO: This does the application of '<|>' on the inner type constructors, whereas the original 
-- implementation for the standard classes applies '<|>' on the outer type constructors.
instance (Applicative f g h, AlternativeAlt f' g' h') => AlternativeAlt (f Generics.:.: f') (g Generics.:.: g') (h Generics.:.: h') where
  type AlternativeAltCts (f Generics.:.: f') (g Generics.:.: g') (h Generics.:.: h') a = ( ApplicativeCts f g h (g' a) (h' a), AlternativeAltCts f' g' h' a
                                                                                         , FunctorCts f (f' a) (g' a -> h' a) )
  (Generics.Comp1 f) <|> (Generics.Comp1 g) = Generics.Comp1 $ fmap (<|>) f <*> g 
instance AlternativeAlt f g h => AlternativeAlt (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h)  where
  type AlternativeAltCts (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h) a = AlternativeAltCts f g h a
  (Generics.M1 f) <|> (Generics.M1 g) = Generics.M1 $ f <|> g















