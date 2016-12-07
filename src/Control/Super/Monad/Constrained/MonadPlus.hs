
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeOperators #-} -- For ':*:' instance and others.

module Control.Super.Monad.Constrained.MonadPlus
  ( MonadPlusZero(..)
  , MonadPlusAdd(..)
  ) where

import qualified Prelude as P
import qualified Control.Monad as M

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
  , Return(..), Bind(..) )
import Control.Super.Monad.Constrained.Alternative 
  ( AlternativeEmpty(..), AlternativeAlt(..) )


class (AlternativeEmpty m, Return m) => MonadPlusZero m where
  type MonadPlusZeroCts m a :: Constraint
  type MonadPlusZeroCts m a = ()
  mzero :: MonadPlusZeroCts m a => m a

instance MonadPlusZero [] where
  mzero = M.mzero
instance MonadPlusZero P.Maybe where
  mzero = M.mzero
instance MonadPlusZero P.IO where
  mzero = M.mzero
instance MonadPlusZero ReadP.ReadP where
  mzero = M.mzero
instance MonadPlusZero ReadPrec.ReadPrec where
  mzero = M.mzero
instance MonadPlusZero STM.STM where
  mzero = M.mzero
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusZero Semigroup.Option where
  mzero = M.mzero
#endif
instance MonadPlusZero Proxy.Proxy where
  mzero = M.mzero
instance (MonadPlusZero f) => MonadPlusZero (Mon.Alt f) where
  type MonadPlusZeroCts (Mon.Alt f) a = MonadPlusZeroCts f a
  mzero = Mon.Alt $ mzero

instance (MonadPlusZero f, MonadPlusZero f') => MonadPlusZero (Product.Product f f') where
  type MonadPlusZeroCts (Product.Product f f') a = (MonadPlusZeroCts f a, MonadPlusZeroCts f' a)
  mzero = Product.Pair mzero mzero

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance MonadPlusZero Generics.U1 where
  mzero = M.mzero
instance MonadPlusZero f => MonadPlusZero (Generics.Rec1 f) where
  type MonadPlusZeroCts (Generics.Rec1 f) a = MonadPlusZeroCts f a
  mzero = Generics.Rec1 mzero
instance (MonadPlusZero f, MonadPlusZero g) => MonadPlusZero (f Generics.:*: g) where
  type MonadPlusZeroCts (f Generics.:*: g) a = (MonadPlusZeroCts f a, MonadPlusZeroCts g a)
  mzero = mzero Generics.:*: mzero
instance MonadPlusZero f => MonadPlusZero (Generics.M1 i c f) where
  type MonadPlusZeroCts (Generics.M1 i c f) a = MonadPlusZeroCts f a
  mzero = Generics.M1 $ mzero


class (AlternativeAlt f g h, Bind f g h) => MonadPlusAdd f g h where
  type MonadPlusAddCts f g h a :: Constraint
  type MonadPlusAddCts f g h a = ()
  mplus :: MonadPlusAddCts f g h a => f a -> g a -> h a

instance MonadPlusAdd [] [] [] where
  mplus = M.mplus
instance MonadPlusAdd P.Maybe P.Maybe P.Maybe where
  mplus = M.mplus
instance MonadPlusAdd P.IO P.IO P.IO where
  mplus = M.mplus
instance MonadPlusAdd ReadP.ReadP ReadP.ReadP ReadP.ReadP where
  mplus = M.mplus
instance MonadPlusAdd ReadPrec.ReadPrec ReadPrec.ReadPrec ReadPrec.ReadPrec where
  mplus = M.mplus
instance MonadPlusAdd STM.STM STM.STM STM.STM where
  mplus = M.mplus
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusAdd Semigroup.Option Semigroup.Option Semigroup.Option where
  mplus = M.mplus
#endif
instance MonadPlusAdd Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  mplus = M.mplus
instance (MonadPlusAdd f g h) => MonadPlusAdd (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) where
  type MonadPlusAddCts (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) a = MonadPlusAddCts f g h a
  mplus (Mon.Alt ma) (Mon.Alt na) = Mon.Alt $ mplus ma na

instance (MonadPlusAdd f g h, MonadPlusAdd f' g' h') => MonadPlusAdd (Product.Product f f') (Product.Product g g') (Product.Product h h') where
  type MonadPlusAddCts (Product.Product f f') (Product.Product g g') (Product.Product h h') a = (MonadPlusAddCts f g h a, MonadPlusAddCts f' g' h' a)
  mplus (Product.Pair m1 m2) (Product.Pair n1 n2) = Product.Pair (mplus m1 n1) (mplus m2 n2)

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

instance MonadPlusAdd Generics.U1 Generics.U1 Generics.U1 where
  mplus = M.mplus
instance MonadPlusAdd f g h => MonadPlusAdd (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) where
  type MonadPlusAddCts (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) a = MonadPlusAddCts f g h a
  mplus (Generics.Rec1 f) (Generics.Rec1 g) = Generics.Rec1 $ mplus f g
instance (MonadPlusAdd f g h, MonadPlusAdd f' g' h') => MonadPlusAdd (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') where
  type MonadPlusAddCts (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') a = (MonadPlusAddCts f g h a, MonadPlusAddCts f' g' h' a)
  mplus (f Generics.:*: g) (f' Generics.:*: g') = (mplus f f') Generics.:*: (mplus g g')
instance MonadPlusAdd f g h => MonadPlusAdd (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h)  where
  type MonadPlusAddCts (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h) a = MonadPlusAddCts f g h a
  mplus (Generics.M1 f) (Generics.M1 g) = Generics.M1 $ mplus f g























