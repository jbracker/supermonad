
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeOperators #-}

-- | __WARNING:__ This module is an experiment to see how 'Alternative' may be encoded.
--   The authors are not aware of any generalized applicatives that make use of 'Alternative'. 
--   Hence, we do not know if this encoding of it is sufficient. 
--   Therefore, the encoding is not in its final form and may change in the future.
module Control.Super.Monad.Alternative
  ( AlternativeEmpty(..)
  , AlternativeAlt(..)
  ) where

import qualified Prelude as P
import qualified Control.Applicative as A

import GHC.Exts ( Constraint )

import qualified GHC.Conc as STM
--import qualified Control.Arrow as Arrow
--import qualified Control.Applicative as Applic
import qualified Data.Monoid as Mon
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import qualified GHC.Generics as Generics
import qualified Data.Semigroup as Semigroup
import qualified Data.Proxy as Proxy
import qualified Data.Functor.Product as Product ( Product(..) )
import qualified Data.Functor.Compose as Compose ( Compose(..) )
#endif

import Control.Super.Monad.Prelude 
  ( ($)
  , Return(..), Applicative(..), Functor(..) )
-- Import of 'Functor' required for GHC 8+ instances.

-- | The encoding of the 'empty' operation.
--  
--   'Return' is not a superclass, because the indices or constraints involved 
--   in an 'AlternativeEmpty' instance may differ from those involved with the 'Return'
--   instance.
--   
--   __WARNING:__ This module is an experiment to see how 'Alternative' may be encoded.
--   The authors are not aware of any generalized applicatives that make use of 'Alternative'. 
--   Hence, we do not know if this encoding of it is sufficient. 
--   Therefore, the encoding is not in its final form and may change in the future.
class (Functor f) => AlternativeEmpty f where
  type AlternativeEmptyCts f :: Constraint
  type AlternativeEmptyCts f = ()
  empty :: AlternativeEmptyCts f => f a

instance AlternativeEmpty [] where
  empty = A.empty
instance AlternativeEmpty P.Maybe where
  empty = A.empty
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeEmpty P.IO where
  empty = A.empty
#endif
instance AlternativeEmpty ReadP.ReadP where
  empty = A.empty
instance AlternativeEmpty ReadPrec.ReadPrec where
  empty = A.empty
instance AlternativeEmpty STM.STM where
  empty = A.empty
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeEmpty Semigroup.Option where
  empty = A.empty
instance AlternativeEmpty Proxy.Proxy where
  empty = A.empty
#endif
instance (AlternativeEmpty f) => AlternativeEmpty (Mon.Alt f) where
  type AlternativeEmptyCts (Mon.Alt f) = AlternativeEmptyCts f
  empty = Mon.Alt $ empty

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance (AlternativeEmpty f, AlternativeEmpty f') => AlternativeEmpty (Product.Product f f') where
  type AlternativeEmptyCts (Product.Product f f') = (AlternativeEmptyCts f, AlternativeEmptyCts f')
  empty = Product.Pair empty empty

instance (AlternativeEmpty f, AlternativeEmpty f') => AlternativeEmpty (Compose.Compose f f') where
  type AlternativeEmptyCts (Compose.Compose f f') = (AlternativeEmptyCts f, AlternativeEmptyCts f')
  empty = Compose.Compose $ empty
#endif

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeEmpty Generics.U1 where
  empty = A.empty
instance AlternativeEmpty f => AlternativeEmpty (Generics.Rec1 f) where
  type AlternativeEmptyCts (Generics.Rec1 f) = AlternativeEmptyCts f
  empty = Generics.Rec1 empty
instance (AlternativeEmpty f, AlternativeEmpty g) => AlternativeEmpty (f Generics.:*: g) where
  type AlternativeEmptyCts (f Generics.:*: g) = (AlternativeEmptyCts f, AlternativeEmptyCts g)
  empty = empty Generics.:*: empty
instance (AlternativeEmpty f, AlternativeEmpty g) => AlternativeEmpty (f Generics.:.: g) where
  type AlternativeEmptyCts (f Generics.:.: g) = (AlternativeEmptyCts f, AlternativeEmptyCts g)
  empty = Generics.Comp1 $ empty
instance AlternativeEmpty f => AlternativeEmpty (Generics.M1 i c f) where
  type AlternativeEmptyCts (Generics.M1 i c f) = AlternativeEmptyCts f
  empty = Generics.M1 $ empty
#endif

-- | The encoding of the '<|>' operation.
--  
--   'Applicative' is not a superclass, because the indices or constraints involved 
--   in an 'Alternative' instance may differ from those involved with the 'Applicative'
--   instance.
--   
--   __WARNING:__ This module is an experiment to see how 'Alternative' may be encoded.
--   The authors are not aware of any generalized applicatives that make use of 'Alternative'. 
--   Hence, we do not know if this encoding of it is sufficient. 
--   Therefore, the encoding is not in its final form and may change in the future.
class (Functor f, Functor g, Functor h) => AlternativeAlt f g h where
  type AlternativeAltCts f g h :: Constraint
  type AlternativeAltCts f g h = ()
  (<|>) :: AlternativeAltCts f g h => f a -> g a -> h a

instance AlternativeAlt [] [] [] where
  (<|>) = (A.<|>)
instance AlternativeAlt P.Maybe P.Maybe P.Maybe where
  (<|>) = (A.<|>)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeAlt P.IO P.IO P.IO where
  (<|>) = (A.<|>)
#endif
instance AlternativeAlt ReadP.ReadP ReadP.ReadP ReadP.ReadP where
  (<|>) = (A.<|>)
instance AlternativeAlt ReadPrec.ReadPrec ReadPrec.ReadPrec ReadPrec.ReadPrec where
  (<|>) = (A.<|>)
instance AlternativeAlt STM.STM STM.STM STM.STM where
  (<|>) = (A.<|>)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeAlt Semigroup.Option Semigroup.Option Semigroup.Option where
  (<|>) = (A.<|>)
instance AlternativeAlt Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (<|>) = (A.<|>)
#endif
instance (AlternativeAlt f g h) => AlternativeAlt (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) where
  type AlternativeAltCts (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) = AlternativeAltCts f g h
  (Mon.Alt ma) <|> (Mon.Alt na) = Mon.Alt $ ma <|> na

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance (AlternativeAlt f g h, AlternativeAlt f' g' h') => AlternativeAlt (Product.Product f f') (Product.Product g g') (Product.Product h h') where
  type AlternativeAltCts (Product.Product f f') (Product.Product g g') (Product.Product h h') = (AlternativeAltCts f g h, AlternativeAltCts f' g' h')
  Product.Pair m1 m2 <|> Product.Pair n1 n2 = Product.Pair (m1 <|> n1) (m2 <|> n2)

-- TODO: This does the application of '<|>' on the inner type constructors, whereas the original 
-- implementation for the standard classes applies '<|>' on the outer type constructors.
instance (Applicative f g h, AlternativeAlt f' g' h') => AlternativeAlt (Compose.Compose f f') (Compose.Compose g g') (Compose.Compose h h') where
  type AlternativeAltCts (Compose.Compose f f') (Compose.Compose g g') (Compose.Compose h h') = (ApplicativeCts f g h, AlternativeAltCts f' g' h')
  (Compose.Compose f) <|> (Compose.Compose g) = Compose.Compose $ fmap (<|>) f <*> g 
#endif

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance AlternativeAlt Generics.U1 Generics.U1 Generics.U1 where
  (<|>) = (A.<|>)
instance AlternativeAlt f g h => AlternativeAlt (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) where
  type AlternativeAltCts (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) = AlternativeAltCts f g h
  (Generics.Rec1 f) <|> (Generics.Rec1 g) = Generics.Rec1 $ f <|> g
instance (AlternativeAlt f g h, AlternativeAlt f' g' h') => AlternativeAlt (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') where
  type AlternativeAltCts (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') = (AlternativeAltCts f g h, AlternativeAltCts f' g' h')
  (f Generics.:*: g) <|> (f' Generics.:*: g') = (f <|> f') Generics.:*: (g <|> g')
-- TODO: This does the application of '<|>' on the inner type constructors, whereas the original 
-- implementation for the standard classes applies '<|>' on the outer type constructors.
instance (Applicative f g h, AlternativeAlt f' g' h') => AlternativeAlt (f Generics.:.: f') (g Generics.:.: g') (h Generics.:.: h') where
  type AlternativeAltCts (f Generics.:.: f') (g Generics.:.: g') (h Generics.:.: h') = (ApplicativeCts f g h, AlternativeAltCts f' g' h')
  (Generics.Comp1 f) <|> (Generics.Comp1 g) = Generics.Comp1 $ fmap (<|>) f <*> g 
instance AlternativeAlt f g h => AlternativeAlt (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h)  where
  type AlternativeAltCts (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h) = AlternativeAltCts f g h
  (Generics.M1 f) <|> (Generics.M1 g) = Generics.M1 $ f <|> g
#endif






















