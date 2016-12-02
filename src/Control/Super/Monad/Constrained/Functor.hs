
{-# LANGUAGE CPP #-}

{-# LANGUAGE ConstraintKinds #-} -- 'Functor' class.
{-# LANGUAGE TypeFamilies    #-} -- 'Functor' class.

{-# LANGUAGE UndecidableInstances #-} -- Required for some of the 'transformer' instances.

{-# LANGUAGE TypeOperators #-} -- For ':*:' instance and others.

-- | Definition of constrained functors as they are required to work with
--   constrained monads and constrained supermonads.
module Control.Super.Monad.Constrained.Functor 
  ( Functor(..)
  ) where

import Prelude
  ( Ord
  , (.), ($), const
  )

import GHC.Exts ( Constraint )

import qualified Prelude as P

-- To define instances:
import Data.Functor.Identity ( Identity )

import qualified Data.Monoid as Mon ( First, Last, Sum, Product, Dual, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )
import qualified Data.Complex as Complex ( Complex )
import qualified Data.Functor.Product as Product ( Product(..) )
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import qualified Data.Semigroup as Semigroup ( Min, Max, Option, First, Last )
import qualified Data.List.NonEmpty as NonEmpty ( NonEmpty )
#endif

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )
import qualified GHC.Generics as Generics

-- To defined constrained instances:
import qualified Data.Set as S

-- To define "transformers" instances:
import qualified Control.Monad.Trans.Cont     as Cont     ( ContT(..) )
import qualified Control.Monad.Trans.Except   as Except   ( ExceptT(..), runExceptT )
import qualified Control.Monad.Trans.Identity as Identity ( IdentityT(..), mapIdentityT )
import qualified Control.Monad.Trans.List     as List     ( ListT(..), mapListT )
import qualified Control.Monad.Trans.Maybe    as Maybe    ( MaybeT(..), mapMaybeT )
import qualified Control.Monad.Trans.RWS.Lazy      as RWSL    ( RWST(..) )
import qualified Control.Monad.Trans.RWS.Strict    as RWSS    ( RWST(..) )
import qualified Control.Monad.Trans.Reader        as Reader  ( ReaderT(..), mapReaderT )
import qualified Control.Monad.Trans.State.Lazy    as StateL  ( StateT(..) )
import qualified Control.Monad.Trans.State.Strict  as StateS  ( StateT(..) )
import qualified Control.Monad.Trans.Writer.Lazy   as WriterL ( WriterT(..), mapWriterT )
import qualified Control.Monad.Trans.Writer.Strict as WriterS ( WriterT(..), mapWriterT )

infixl 4  <$

-- -----------------------------------------------------------------------------
-- Constrained functor class
-- -----------------------------------------------------------------------------

-- | Class for constrained functors. Obeys all of the same laws as the standard
--   'Prelude.Functor' class, but allows to constrain the functors result type.
class Functor f where
  type FunctorCts f (a :: *) (b :: *) :: Constraint
  type FunctorCts f a b = ()
  
  fmap :: (FunctorCts f a b) => (a -> b) -> f a -> f b
  (<$) :: (FunctorCts f b a) => a -> f b -> f a
  (<$) = fmap . const

-- Unconstrained instances -----------------------------------------------------

instance Functor ((->) r) where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Identity where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor [] where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor P.Maybe where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor P.IO where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor (P.Either e) where
  fmap = P.fmap
  (<$) = (P.<$)

instance Functor Mon.First where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Mon.Last where
  fmap = P.fmap
  (<$) = (P.<$)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Functor Mon.Sum where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Mon.Product where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Mon.Dual where
  fmap = P.fmap
  (<$) = (P.<$)
#endif
instance (Functor f) => Functor (Mon.Alt f) where
  type FunctorCts (Mon.Alt f) a b = FunctorCts f a b
  fmap f (Mon.Alt ma) = Mon.Alt $ fmap f ma
  a <$ (Mon.Alt mb) = Mon.Alt $ a <$ mb

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Functor Semigroup.Min where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Semigroup.Max where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Semigroup.Option where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Semigroup.First where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Semigroup.Last where
  fmap = P.fmap
  (<$) = (P.<$)
#endif

instance Functor Proxy.Proxy where
  fmap = P.fmap
  (<$) = (P.<$)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Functor Complex.Complex where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor NonEmpty.NonEmpty where
  fmap = P.fmap
  (<$) = (P.<$)
#endif
instance (Functor f, Functor g) => Functor (Product.Product f g) where
  type FunctorCts (Product.Product f g) a b = (FunctorCts f a b, FunctorCts g a b)
  fmap f (Product.Pair fa fb) = Product.Pair (fmap f fa) (fmap f fb)

instance Functor Read.ReadP where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor Read.ReadPrec where
  fmap = P.fmap
  (<$) = (P.<$)

instance Functor (ST.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance Functor (STL.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (Arrow.ArrowApply a) => Functor (Arrow.ArrowMonad a) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (Functor m) => Functor (App.WrappedMonad m) where
  type FunctorCts (App.WrappedMonad m) a b = FunctorCts m a b
  fmap f (App.WrapMonad ma) = App.WrapMonad $ fmap f ma
  a <$ (App.WrapMonad mb) = App.WrapMonad $ a <$ mb

instance Functor STM.STM where
  fmap = P.fmap
  (<$) = (P.<$)

instance Functor Generics.U1 where
  fmap = P.fmap
  (<$) = (P.<$)
instance (Functor f) => Functor (Generics.Rec1 f) where
  type FunctorCts (Generics.Rec1 f) a b = FunctorCts f a b
  fmap f (Generics.Rec1 ma) = Generics.Rec1 $ fmap f ma
  a <$ (Generics.Rec1 mb) = Generics.Rec1 $ a <$ mb

instance (Functor f, Functor g) => Functor (f Generics.:*: g) where
  type FunctorCts (f Generics.:*: g) a b = (FunctorCts f a b, FunctorCts g a b)
  fmap f (a Generics.:*: b) = (fmap f a) Generics.:*: (fmap f b)
instance (Functor f, Functor g) => Functor (f Generics.:.: g) where
  type FunctorCts (f Generics.:.: g) a b = (FunctorCts f (g a) (g b), FunctorCts g a b)
  fmap f (Generics.Comp1 ma) = Generics.Comp1 $ fmap (fmap f) ma

-- Constrained instances -------------------------------------------------------

instance Functor S.Set where
  type FunctorCts S.Set a b = Ord b
  fmap = S.map

-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance Functor (Cont.ContT r m) where
  fmap = P.fmap
  (<$) = (P.<$)

instance Functor m => Functor (Except.ExceptT e m) where
  type FunctorCts (Except.ExceptT e m) a b = FunctorCts m (P.Either e a) (P.Either e b)
  fmap f = Except.ExceptT . fmap (fmap f) . Except.runExceptT
  {-# INLINE fmap #-}

instance (Functor m) => Functor (Identity.IdentityT m) where
  type FunctorCts (Identity.IdentityT m) a b = FunctorCts m a b
  fmap f = Identity.mapIdentityT (fmap f)
  {-# INLINE fmap #-}

instance (Functor m) => Functor (List.ListT m) where
  type FunctorCts (List.ListT m) a b = FunctorCts m [a] [b]
  fmap f = List.mapListT $ fmap $ P.map f
  {-# INLINE fmap #-}

instance (Functor m) => Functor (Maybe.MaybeT m) where
  type FunctorCts (Maybe.MaybeT m) a b = FunctorCts m (P.Maybe a) (P.Maybe b)
  fmap f = Maybe.mapMaybeT (fmap (fmap f))
  {-# INLINE fmap #-}

instance (Functor m) => Functor (RWSL.RWST r w s m) where
  type FunctorCts (RWSL.RWST r w s m) a b = FunctorCts m (a, s, w) (b, s, w)
  fmap f m = RWSL.RWST $ \ r s ->
      fmap (\ ~(a, s', w) -> (f a, s', w)) $ RWSL.runRWST m r s
  {-# INLINE fmap #-}

instance (Functor m) => Functor (RWSS.RWST r w s m) where
  type FunctorCts (RWSS.RWST r w s m) a b = FunctorCts m (a, s, w) (b, s, w)
  fmap f m = RWSS.RWST $ \ r s ->
      fmap (\ (a, s', w) -> (f a, s', w)) $ RWSS.runRWST m r s
  {-# INLINE fmap #-}

instance (Functor m) => Functor (Reader.ReaderT r m) where
  type FunctorCts (Reader.ReaderT r m) a b = FunctorCts m a b
  fmap f  = Reader.mapReaderT (fmap f)
  {-# INLINE fmap #-}

instance (Functor m) => Functor (StateL.StateT s m) where
  type FunctorCts (StateL.StateT s m) a b = FunctorCts m (a, s) (b, s)
  fmap f m = StateL.StateT $ \ s ->
      fmap (\ ~(a, s') -> (f a, s')) $ StateL.runStateT m s
  {-# INLINE fmap #-}

instance (Functor m) => Functor (StateS.StateT s m) where
  type FunctorCts (StateS.StateT s m) a b = FunctorCts m (a, s) (b, s)
  fmap f m = StateS.StateT $ \ s ->
      fmap (\ (a, s') -> (f a, s')) $ StateS.runStateT m s
  {-# INLINE fmap #-}

instance (Functor m) => Functor (WriterL.WriterT w m) where
  type FunctorCts (WriterL.WriterT w m) a b = FunctorCts m (a, w) (b, w)
  fmap f = WriterL.mapWriterT $ fmap $ \ ~(a, w) -> (f a, w)
  {-# INLINE fmap #-}

instance (Functor m) => Functor (WriterS.WriterT w m) where
  type FunctorCts (WriterS.WriterT w m) a b = FunctorCts m (a, w) (b, w)
  fmap f = WriterS.mapWriterT $ fmap $ \ (a, w) -> (f a, w)
  {-# INLINE fmap #-}













