
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeOperators #-} -- For ':*:' instance and others.

-- | __WARNING:__ This module is an experiment to see how 'MonadPlus' may be encoded.
--   The authors are not aware of any generalized monads that make use of 'MonadPlus'. 
--   Hence, we do not know if this encoding of it is sufficient. 
--   Therefore, the encoding is not in its final form and may change in the future.
module Control.Super.Monad.MonadPlus
  ( MonadPlusZero(..)
  , MonadPlusAdd(..)
  ) where

import qualified Prelude as P
import qualified Control.Monad as M

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
import qualified Data.Functor.Product as Product
#endif

import Control.Super.Monad.Prelude 
  ( ($)
  , Return(..), Bind(..) )

-- | The encoding of the 'mzero' operation.
class (Return m) => MonadPlusZero m where
  type MonadPlusZeroCts m :: Constraint
  type MonadPlusZeroCts m = ()
  mzero :: MonadPlusZeroCts m => m a

instance MonadPlusZero [] where
  mzero = M.mzero
instance MonadPlusZero P.Maybe where
  mzero = M.mzero
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusZero P.IO where
  mzero = M.mzero
#endif
instance MonadPlusZero ReadP.ReadP where
  mzero = M.mzero
instance MonadPlusZero ReadPrec.ReadPrec where
  mzero = M.mzero
instance MonadPlusZero STM.STM where
  mzero = M.mzero
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusZero Semigroup.Option where
  mzero = M.mzero
instance MonadPlusZero Proxy.Proxy where
  mzero = M.mzero
#endif
instance (MonadPlusZero f) => MonadPlusZero (Mon.Alt f) where
  type MonadPlusZeroCts (Mon.Alt f) = MonadPlusZeroCts f
  mzero = Mon.Alt $ mzero

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance (MonadPlusZero f, MonadPlusZero f') => MonadPlusZero (Product.Product f f') where
  type MonadPlusZeroCts (Product.Product f f') = (MonadPlusZeroCts f, MonadPlusZeroCts f')
  mzero = Product.Pair mzero mzero
#endif

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusZero Generics.U1 where
  mzero = M.mzero
instance MonadPlusZero f => MonadPlusZero (Generics.Rec1 f) where
  type MonadPlusZeroCts (Generics.Rec1 f) = MonadPlusZeroCts f
  mzero = Generics.Rec1 mzero
instance (MonadPlusZero f, MonadPlusZero g) => MonadPlusZero (f Generics.:*: g) where
  type MonadPlusZeroCts (f Generics.:*: g) = (MonadPlusZeroCts f, MonadPlusZeroCts g)
  mzero = mzero Generics.:*: mzero
instance MonadPlusZero f => MonadPlusZero (Generics.M1 i c f) where
  type MonadPlusZeroCts (Generics.M1 i c f) = MonadPlusZeroCts f
  mzero = Generics.M1 $ mzero
#endif

-- | The encoding of the 'mplus' operation
class (Bind f g h) => MonadPlusAdd f g h where
  type MonadPlusAddCts f g h :: Constraint
  type MonadPlusAddCts f g h = ()
  mplus :: MonadPlusAddCts f g h => f a -> g a -> h a

instance MonadPlusAdd [] [] [] where
  mplus = M.mplus
instance MonadPlusAdd P.Maybe P.Maybe P.Maybe where
  mplus = M.mplus
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusAdd P.IO P.IO P.IO where
  mplus = M.mplus
#endif
instance MonadPlusAdd ReadP.ReadP ReadP.ReadP ReadP.ReadP where
  mplus = M.mplus
instance MonadPlusAdd ReadPrec.ReadPrec ReadPrec.ReadPrec ReadPrec.ReadPrec where
  mplus = M.mplus
instance MonadPlusAdd STM.STM STM.STM STM.STM where
  mplus = M.mplus
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusAdd Semigroup.Option Semigroup.Option Semigroup.Option where
  mplus = M.mplus
instance MonadPlusAdd Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  mplus = M.mplus
#endif
instance (MonadPlusAdd f g h) => MonadPlusAdd (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) where
  type MonadPlusAddCts (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) = MonadPlusAddCts f g h
  mplus (Mon.Alt ma) (Mon.Alt na) = Mon.Alt $ mplus ma na

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance (MonadPlusAdd f g h, MonadPlusAdd f' g' h') => MonadPlusAdd (Product.Product f f') (Product.Product g g') (Product.Product h h') where
  type MonadPlusAddCts (Product.Product f f') (Product.Product g g') (Product.Product h h') = (MonadPlusAddCts f g h, MonadPlusAddCts f' g' h')
  mplus (Product.Pair m1 m2) (Product.Pair n1 n2) = Product.Pair (mplus m1 n1) (mplus m2 n2)
#endif

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance MonadPlusAdd Generics.U1 Generics.U1 Generics.U1 where
  mplus = M.mplus
instance MonadPlusAdd f g h => MonadPlusAdd (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) where
  type MonadPlusAddCts (Generics.Rec1 f) (Generics.Rec1 g) (Generics.Rec1 h) = MonadPlusAddCts f g h
  mplus (Generics.Rec1 f) (Generics.Rec1 g) = Generics.Rec1 $ mplus f g
instance (MonadPlusAdd f g h, MonadPlusAdd f' g' h') => MonadPlusAdd (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') where
  type MonadPlusAddCts (f Generics.:*: f') (g Generics.:*: g') (h Generics.:*: h') = (MonadPlusAddCts f g h, MonadPlusAddCts f' g' h')
  mplus (f Generics.:*: g) (f' Generics.:*: g') = (mplus f f') Generics.:*: (mplus g g')
instance MonadPlusAdd f g h => MonadPlusAdd (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h)  where
  type MonadPlusAddCts (Generics.M1 i c f) (Generics.M1 i c g) (Generics.M1 i c h) = MonadPlusAddCts f g h
  mplus (Generics.M1 f) (Generics.M1 g) = Generics.M1 $ mplus f g
#endif






















