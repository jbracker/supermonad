
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeOperators #-} -- For ':*:' instance and others.

module Control.Super.Monad.MonadPlus
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
import qualified Data.Semigroup as Semigroup
import qualified Data.Proxy as Proxy
import qualified Data.Monoid as Mon
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Control.Super.Monad.Prelude 
  ( ($)
  , Return(..), Bind(..) )
import Control.Super.Monad.Alternative 
  ( AlternativeEmpty(..), AlternativeAlt(..) )


class (AlternativeEmpty m, Return m) => MonadPlusZero m where
  type MonadPlusZeroCts m :: Constraint
  type MonadPlusZeroCts m = ()
  mzero :: MonadPlusZeroCts m => m a

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
instance MonadPlusZero Semigroup.Option where
  mzero = M.mzero
instance MonadPlusZero Proxy.Proxy where
  mzero = M.mzero
instance (MonadPlusZero f) => MonadPlusZero (Mon.Alt f) where
  type MonadPlusZeroCts (Mon.Alt f) = MonadPlusZeroCts f
  mzero = Mon.Alt $ mzero

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

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



class (AlternativeAlt f g h, Bind f g h) => MonadPlusAdd f g h where
  type MonadPlusAddCts f g h :: Constraint
  type MonadPlusAddCts f g h = ()
  mplus :: MonadPlusAddCts f g h => f a -> g a -> h a

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
instance MonadPlusAdd Semigroup.Option Semigroup.Option Semigroup.Option where
  mplus = M.mplus
instance MonadPlusAdd Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  mplus = M.mplus
instance (MonadPlusAdd f g h) => MonadPlusAdd (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) where
  type MonadPlusAddCts (Mon.Alt f) (Mon.Alt g) (Mon.Alt h) = MonadPlusAddCts f g h
  mplus (Mon.Alt ma) (Mon.Alt na) = Mon.Alt $ mplus ma na

-- TODO: ArrowMonad and WrappedMonad instances. These lead to cyclic dependencies.

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























