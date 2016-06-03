
{-# LANGUAGE ConstraintKinds #-} -- 'CFunctor' class.
{-# LANGUAGE TypeFamilies    #-} -- 'CFunctor' class.

{-# LANGUAGE UndecidableInstances #-} -- Required for some of the 'transformer' instances.

-- | Definition of constrained functors as they are required to work with
--   constrained monads and constrained supermonads.
module Control.Supermonad.Constrained.Functor 
  ( CFunctor(..)
  ) where

import Prelude
  ( Ord
  , (.), ($), const
  )

import GHC.Exts ( Constraint )

import qualified Prelude as P

-- To define instances:
import Data.Functor.Identity ( Identity )

import qualified Data.Monoid as Mon ( First, Last, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )

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

-- -----------------------------------------------------------------------------
-- Constrained functor class
-- -----------------------------------------------------------------------------

-- | Class for constrained functors. Obeys all of the same laws as the standard
--   'Functor' class, but allows to constrain the functors result type.
class CFunctor f where
  type CFunctorCts f (a :: *) (b :: *) :: Constraint
  type CFunctorCts f a b = ()
  
  fmap :: (CFunctorCts f a b) => (a -> b) -> f a -> f b
  (<$) :: (CFunctorCts f b a) => a -> f b -> f a
  (<$) = fmap . const

-- Unconstrained instances -----------------------------------------------------

instance CFunctor Identity where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor [] where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor P.Maybe where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor P.IO where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor (P.Either e) where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor Mon.First where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor Mon.Last where
  fmap = P.fmap
  (<$) = (P.<$)
instance (CFunctor f) => CFunctor (Mon.Alt f) where
  type CFunctorCts (Mon.Alt f) a b = CFunctorCts f a b
  fmap f (Mon.Alt ma) = Mon.Alt $ fmap f ma
  a <$ (Mon.Alt mb) = Mon.Alt $ a <$ mb

instance CFunctor Proxy.Proxy where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor Read.ReadP where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor Read.ReadPrec where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor (ST.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance CFunctor (STL.ST s) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (Arrow.ArrowApply a) => CFunctor (Arrow.ArrowMonad a) where
  fmap = P.fmap
  (<$) = (P.<$)
instance (CFunctor m) => CFunctor (App.WrappedMonad m) where
  type CFunctorCts (App.WrappedMonad m) a b = CFunctorCts m a b
  fmap f (App.WrapMonad ma) = App.WrapMonad $ fmap f ma
  a <$ (App.WrapMonad mb) = App.WrapMonad $ a <$ mb

instance CFunctor STM.STM where
  fmap = P.fmap
  (<$) = (P.<$)

-- Constrained instances -------------------------------------------------------

instance CFunctor S.Set where
  type CFunctorCts S.Set a b = Ord b
  fmap = S.map

-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance CFunctor (Cont.ContT r m) where
  fmap = P.fmap
  (<$) = (P.<$)

instance CFunctor m => CFunctor (Except.ExceptT e m) where
  type CFunctorCts (Except.ExceptT e m) a b = CFunctorCts m (P.Either e a) (P.Either e b)
  fmap f = Except.ExceptT . fmap (fmap f) . Except.runExceptT
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (Identity.IdentityT m) where
  type CFunctorCts (Identity.IdentityT m) a b = CFunctorCts m a b
  fmap f = Identity.mapIdentityT (fmap f)
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (List.ListT m) where
  type CFunctorCts (List.ListT m) a b = CFunctorCts m [a] [b]
  fmap f = List.mapListT $ fmap $ P.map f
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (Maybe.MaybeT m) where
  type CFunctorCts (Maybe.MaybeT m) a b = CFunctorCts m (P.Maybe a) (P.Maybe b)
  fmap f = Maybe.mapMaybeT (fmap (fmap f))
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (RWSL.RWST r w s m) where
  type CFunctorCts (RWSL.RWST r w s m) a b = CFunctorCts m (a, s, w) (b, s, w)
  fmap f m = RWSL.RWST $ \ r s ->
      fmap (\ ~(a, s', w) -> (f a, s', w)) $ RWSL.runRWST m r s
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (RWSS.RWST r w s m) where
  type CFunctorCts (RWSS.RWST r w s m) a b = CFunctorCts m (a, s, w) (b, s, w)
  fmap f m = RWSS.RWST $ \ r s ->
      fmap (\ (a, s', w) -> (f a, s', w)) $ RWSS.runRWST m r s
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (Reader.ReaderT r m) where
  type CFunctorCts (Reader.ReaderT r m) a b = CFunctorCts m a b
  fmap f  = Reader.mapReaderT (fmap f)
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (StateL.StateT s m) where
  type CFunctorCts (StateL.StateT s m) a b = CFunctorCts m (a, s) (b, s)
  fmap f m = StateL.StateT $ \ s ->
      fmap (\ ~(a, s') -> (f a, s')) $ StateL.runStateT m s
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (StateS.StateT s m) where
  type CFunctorCts (StateS.StateT s m) a b = CFunctorCts m (a, s) (b, s)
  fmap f m = StateS.StateT $ \ s ->
      fmap (\ (a, s') -> (f a, s')) $ StateS.runStateT m s
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (WriterL.WriterT w m) where
  type CFunctorCts (WriterL.WriterT w m) a b = CFunctorCts m (a, w) (b, w)
  fmap f = WriterL.mapWriterT $ fmap $ \ ~(a, w) -> (f a, w)
  {-# INLINE fmap #-}

instance (CFunctor m) => CFunctor (WriterS.WriterT w m) where
  type CFunctorCts (WriterS.WriterT w m) a b = CFunctorCts m (a, w) (b, w)
  fmap f = WriterS.mapWriterT $ fmap $ \ (a, w) -> (f a, w)
  {-# INLINE fmap #-}













