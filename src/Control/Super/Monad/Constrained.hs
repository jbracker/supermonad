
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses  #-} -- for 'Bind' class.
{-# LANGUAGE ConstraintKinds        #-} -- for 'Bind' class.
{-# LANGUAGE TypeFamilies           #-} -- for 'Bind' class.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE InstanceSigs         #-} -- for 'ListT' instance.
{-# LANGUAGE ScopedTypeVariables  #-} -- for 'ListT' instance.
{-# LANGUAGE UndecidableInstances #-} -- for 'ListT' instance.

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- Some of the constraints may be unnecessary, but they are intentional.
-- This is especially true for the 'Fail' instances.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

-- | Definition of supermonads that support constrained monads.
module Control.Super.Monad.Constrained 
  ( -- * Supermonads
    Bind(..), Return(..), Fail(..)
    -- * Super-Applicatives
  , Applicative(..), pure
  , Functor(..)
    -- * Conveniences
  , Monad
  ) where

import GHC.Exts ( Constraint )

import Prelude
  ( String, Maybe, Either
  , Ord
  , (.), ($), const
  )
import qualified Prelude as P


-- To define instances:
import Data.Functor.Identity ( Identity(..) )

import qualified Data.Monoid as Mon ( First, Last, Sum, Product, Dual, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )
import qualified Data.Complex as Complex ( Complex )
import qualified Data.Functor.Product as Product ( Product(..) )
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import qualified Data.Semigroup as Semigroup ( Min, Max, Option, First, Last )
import qualified Data.List.NonEmpty as NonEmpty ( NonEmpty )
#endif

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply, Arrow )
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
import qualified Control.Monad.Trans.Identity as Identity ( IdentityT(..) )
import qualified Control.Monad.Trans.List     as List     ( ListT(..) )
import qualified Control.Monad.Trans.Maybe    as Maybe    ( MaybeT(..) )
import qualified Control.Monad.Trans.RWS.Lazy      as RWSL    ( RWST(..) )
import qualified Control.Monad.Trans.RWS.Strict    as RWSS    ( RWST(..) )
import qualified Control.Monad.Trans.Reader        as Reader  ( ReaderT(..) )
import qualified Control.Monad.Trans.State.Lazy    as StateL  ( StateT(..) )
import qualified Control.Monad.Trans.State.Strict  as StateS  ( StateT(..) )
import qualified Control.Monad.Trans.Writer.Lazy   as WriterL ( WriterT(..) )
import qualified Control.Monad.Trans.Writer.Strict as WriterS ( WriterT(..) )

-- To define 'Bind' class:
import Control.Super.Monad.Constrained.Functor 
  ( Functor(..) )

-- -----------------------------------------------------------------------------
-- Super-Applicative Type Class
-- -----------------------------------------------------------------------------

infixl 4 <*>, <*, *>

-- | TODO
class (Functor m, Functor n, Functor p) => Applicative m n p where
  type ApplicativeCts m n p (a :: *) (b :: *) :: Constraint
  type ApplicativeCts m n p a b = ()
  
  type ApplicativeCtsR m n p (a :: *) (b :: *) :: Constraint
  type ApplicativeCtsR m n p a b = ApplicativeCts m n p a b
  
  type ApplicativeCtsL m n p (a :: *) (b :: *) :: Constraint
  type ApplicativeCtsL m n p a b = ApplicativeCts m n p a b 
  
  (<*>) :: (ApplicativeCts m n p a b) => m (a -> b) -> n a -> p b
  
  -- TODO: Cannot give standard instances, because they would require 
  -- different constraints.
  (*>) :: (ApplicativeCtsR m n p a b) => m a -> n b -> p b
  --ma *> nb = (P.id <$ ma) <*> nb
  --ma *> nb = (pure P.id <*> ma) <*> nb
  
  (<*) :: (ApplicativeCtsL m n p a b) => m a -> n b -> p a
  --ma <* nb = fmap const ma <*> nb
  --ma <* nb = (pure const <*> ma) <*> nb
  

-- | 'pure' is defined in terms of return.
pure :: (Return f, ReturnCts f a) => a -> f a
pure = return

type family DefaultAppCtsR m n p a b :: Constraint where
  DefaultAppCtsR m n p a b = (ApplicativeCts m n p b b, FunctorCts m a (b -> b))

type family DefaultAppCtsL m n p a b :: Constraint where
  DefaultAppCtsL m n p a b = (ApplicativeCts m n p b a, FunctorCts m a (b -> a))

defaultAppR :: (Applicative m n p, DefaultAppCtsR m n p a b) => m a -> n b -> p b
defaultAppR ma nb = (P.id <$ ma) <*> nb

defaultAppL :: (Applicative m n p, DefaultAppCtsL m n p a b) => m a -> n b -> p a
defaultAppL ma nb = fmap const ma <*> nb

-- Standard Instances ----------------------------------------------------------

instance Applicative ((->) r) ((->) r) ((->) r) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Identity Identity Identity where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative [] [] [] where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Maybe Maybe Maybe where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative P.IO P.IO P.IO where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative (Either e) (Either e) (Either e) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative Mon.First Mon.First Mon.First where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Mon.Last Mon.Last Mon.Last where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Applicative Mon.Sum Mon.Sum Mon.Sum where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Mon.Product Mon.Product Mon.Product where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Mon.Dual Mon.Dual Mon.Dual where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
#endif
instance (Applicative m n p) => Applicative (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) where
  type ApplicativeCts (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) a b = ApplicativeCts m n p a b
  type ApplicativeCtsR (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) a b = ApplicativeCtsR m n p a b
  type ApplicativeCtsL (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) a b = ApplicativeCtsL m n p a b
  mf <*> na = Mon.Alt $ (Mon.getAlt mf) <*> (Mon.getAlt na)
  mf *> na = Mon.Alt $ (Mon.getAlt mf) *> (Mon.getAlt na)
  mf <* na = Mon.Alt $ (Mon.getAlt mf) <* (Mon.getAlt na)

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Applicative Semigroup.Min Semigroup.Min Semigroup.Min where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Semigroup.Max Semigroup.Max Semigroup.Max where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Semigroup.Option Semigroup.Option Semigroup.Option where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Semigroup.First Semigroup.First Semigroup.First where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Semigroup.Last Semigroup.Last Semigroup.Last where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
#endif

instance Applicative Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Applicative Complex.Complex Complex.Complex Complex.Complex where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative NonEmpty.NonEmpty NonEmpty.NonEmpty NonEmpty.NonEmpty where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
#endif
instance (Applicative m1 n1 p1, Applicative m2 n2 p2) => Applicative (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) where
  type ApplicativeCts (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) a b = (ApplicativeCts m1 n1 p1 a b, ApplicativeCts m2 n2 p2 a b)
  type ApplicativeCtsR (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) a b = (ApplicativeCtsR m1 n1 p1 a b, ApplicativeCtsR m2 n2 p2 a b)
  type ApplicativeCtsL (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) a b = (ApplicativeCtsL m1 n1 p1 a b, ApplicativeCtsL m2 n2 p2 a b)
  Product.Pair m1 m2 <*> Product.Pair n1 n2 = Product.Pair (m1 <*> n1) (m2 <*> n2)
  Product.Pair m1 m2 *> Product.Pair n1 n2 = Product.Pair (m1 *> n1) (m2 *> n2)
  Product.Pair m1 m2 <* Product.Pair n1 n2 = Product.Pair (m1 <* n1) (m2 <* n2)

instance Applicative Read.ReadP Read.ReadP Read.ReadP where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative Read.ReadPrec Read.ReadPrec Read.ReadPrec where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance Applicative (ST.ST s) (ST.ST s) (ST.ST s) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
instance Applicative (STL.ST s) (STL.ST s) (STL.ST s) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance (Arrow.Arrow a, Arrow.ArrowApply a) => Applicative (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

instance (Applicative m n p) => Applicative (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) where
  type ApplicativeCts (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) a b = ApplicativeCts m n p a b
  type ApplicativeCtsR (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) a b = ApplicativeCtsR m n p a b
  type ApplicativeCtsL (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) a b = ApplicativeCtsL m n p a b
  mf <*> na = App.WrapMonad $ (App.unwrapMonad mf) <*> (App.unwrapMonad na)
  mf *> na = App.WrapMonad $ (App.unwrapMonad mf) *> (App.unwrapMonad na)
  mf <* na = App.WrapMonad $ (App.unwrapMonad mf) <* (App.unwrapMonad na)

instance Applicative STM.STM STM.STM STM.STM where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

-- Constrained Instances -------------------------------------------------------

instance Applicative S.Set S.Set S.Set where
  type ApplicativeCts S.Set S.Set S.Set a b = Ord b
  type ApplicativeCtsR S.Set S.Set S.Set a b = ()
  type ApplicativeCtsL S.Set S.Set S.Set a b = ()
  fs  <*> as = S.foldr (\f r -> S.map f as `S.union` r) S.empty fs
  as  <*  _bs = as
  _as *>  bs = bs
  
-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance Applicative (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) where
  type ApplicativeCts (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) a b = ()
  f <*> a = Cont.ContT $ \ c -> Cont.runContT f $ \ g -> Cont.runContT a (c . g)
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) where
  type ApplicativeCts (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) a b = 
        ( ApplicativeCts m n p (Either e a) (Either e b)
        , FunctorCts m (Either e (a -> b)) (Either e a -> Either e b) )
  type ApplicativeCtsL (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) a b = 
        (ApplicativeCtsL m n p (Either e a) (Either e b))
  type ApplicativeCtsR (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) a b = 
        (ApplicativeCtsR m n p (Either e a) (Either e b))
  Except.ExceptT f <*> Except.ExceptT v = Except.ExceptT $ fmap (<*>) f <*> v
  Except.ExceptT a <*  Except.ExceptT b = Except.ExceptT $ a <* b
  Except.ExceptT a *>  Except.ExceptT b = Except.ExceptT $ a *> b
  {-# INLINEABLE (<*>) #-}

instance (Applicative m n p) => Applicative (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) where
  type ApplicativeCts  (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) a b = (ApplicativeCts m n p a b)
  type ApplicativeCtsR (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) a b = (ApplicativeCtsR m n p a b)
  type ApplicativeCtsL (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) a b = (ApplicativeCtsL m n p a b)
  Identity.IdentityT m <*> Identity.IdentityT k = Identity.IdentityT $ m <*> k
  Identity.IdentityT a <*  Identity.IdentityT b = Identity.IdentityT $ a <* b
  Identity.IdentityT a *>  Identity.IdentityT b = Identity.IdentityT $ a *> b
  {-# INLINE (<*>) #-}

-- Requires undecidable instances.
instance (Applicative m n p) => Applicative (List.ListT m) (List.ListT n) (List.ListT p) where
  type ApplicativeCts  (List.ListT m) (List.ListT n) (List.ListT p) a b = 
        ( ApplicativeCts m n p [a] [b]
        , FunctorCts m [a -> b] ([a] -> [b]) )
  type ApplicativeCtsR (List.ListT m) (List.ListT n) (List.ListT p) a b = 
        ( ApplicativeCtsR m n p [a] [b] )
  type ApplicativeCtsL (List.ListT m) (List.ListT n) (List.ListT p) a b =
        ( ApplicativeCtsL m n p [a] [b] )
  List.ListT fs <*> List.ListT as = List.ListT $ fmap (<*>) fs <*> as
  List.ListT as <*  List.ListT bs = List.ListT $ as <* bs
  List.ListT as *>  List.ListT bs = List.ListT $ as *> bs
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) where
  type ApplicativeCts  (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) a b = 
        ( ApplicativeCts m n p (Maybe a) (Maybe b)
        , FunctorCts m (Maybe (a -> b)) (Maybe a -> Maybe b) )
  type ApplicativeCtsR (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) a b = 
        ( ApplicativeCtsR m n p (Maybe a) (Maybe b) )
  type ApplicativeCtsL (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) a b =
        ( ApplicativeCtsL m n p (Maybe a) (Maybe b) )
  Maybe.MaybeT f <*> Maybe.MaybeT x = Maybe.MaybeT $ fmap (<*>) f <*> x
  Maybe.MaybeT a <*  Maybe.MaybeT b = Maybe.MaybeT $ a <* b
  Maybe.MaybeT a *>  Maybe.MaybeT b = Maybe.MaybeT $ a *> b
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Bind m n p) => Applicative (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) where
  type ApplicativeCts (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b = 
        ( BindCts m n p (a -> b, s, w) (b, s, w)
        , FunctorCts n (a, s, w) (b, s, w) )
  type ApplicativeCtsR (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b = 
        (DefaultAppCtsR (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b)
  type ApplicativeCtsL (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b = 
        (DefaultAppCtsL (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b)
  RWSL.RWST mf <*> RWSL.RWST ma  = RWSL.RWST $ \r s -> mf r s >>= \ ~(f, s', w) -> fmap (\ ~(a, s'', w') -> (f a, s'', P.mappend w w')) (ma r s')
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Bind m n p) => Applicative (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) where
  type ApplicativeCts (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b = 
        ( BindCts m n p (a -> b, s, w) (b, s, w)
        , FunctorCts n (a, s, w) (b, s, w) )
  type ApplicativeCtsR (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b = 
        (DefaultAppCtsR (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b)
  type ApplicativeCtsL (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b = 
        (DefaultAppCtsL (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b)
  RWSS.RWST mf <*> RWSS.RWST ma = RWSS.RWST $ \r s -> mf r s >>= \ (f, s', w) -> fmap (\ (a, s'', w') -> (f a, s'', P.mappend w w')) (ma r s')
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) where
  type ApplicativeCts (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b = (ApplicativeCts m n p a b)
  type ApplicativeCtsR (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b = 
        (DefaultAppCtsR (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b)
  type ApplicativeCtsL (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b = 
        (DefaultAppCtsL (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b)
  Reader.ReaderT mf <*> Reader.ReaderT ma  = Reader.ReaderT $ \r -> mf r <*> ma r
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (Bind m n p) => Applicative (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) where
  type ApplicativeCts (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b = 
        ( BindCts m n p (a -> b, s) (b, s)
        , FunctorCts n (a, s) (b, s) )
  type ApplicativeCtsR (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b = 
        (DefaultAppCtsR (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b)
  type ApplicativeCtsL (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b = 
        (DefaultAppCtsL (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b)
  StateL.StateT mf <*> StateL.StateT ma = StateL.StateT $ \s -> mf s >>= \ ~(f, s') -> fmap (\ ~(a, s'') -> (f a, s'')) (ma s')
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (Bind m n p) => Applicative (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) where
  type ApplicativeCts (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b = 
        ( BindCts m n p (a -> b, s) (b, s)
        , FunctorCts n (a, s) (b, s) )
  type ApplicativeCtsR (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b = 
        (DefaultAppCtsR (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b)
  type ApplicativeCtsL (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b = 
        (DefaultAppCtsL (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b)
  StateS.StateT mf <*> StateS.StateT ma = StateS.StateT $ \s -> mf s >>= \ (f, s') -> fmap (\ (a, s'') -> (f a, s'')) (ma s')
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Applicative m n p) => Applicative (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) where
  type ApplicativeCts (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b = 
        ( ApplicativeCts m n p (a, w) (b, w)
        , FunctorCts m (a -> b, w) ((a, w) -> (b, w)) )
  type ApplicativeCtsR (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b = 
        (DefaultAppCtsR (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b)
  type ApplicativeCtsL (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b = 
        (DefaultAppCtsL (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b)
  WriterL.WriterT mf <*> WriterL.WriterT ma = WriterL.WriterT $ fmap (\ ~(f, w) ~(a, w') -> (f a, P.mappend w w')) mf <*> ma
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Applicative m n p) => Applicative (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) where
  type ApplicativeCts (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b = 
        ( ApplicativeCts m n p (a, w) (b, w)
        , FunctorCts m (a -> b, w) ((a, w) -> (b, w)) )
  type ApplicativeCtsR (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b = 
        (DefaultAppCtsR (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b)
  type ApplicativeCtsL (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b = 
        (DefaultAppCtsL (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b)
  WriterS.WriterT mf <*> WriterS.WriterT ma = WriterS.WriterT $ fmap (\ (f, w) (a, w') -> (f a, P.mappend w w')) mf <*> ma
  (<*) = defaultAppL
  (*>) = defaultAppR
  {-# INLINE (<*>) #-}
  
-- -----------------------------------------------------------------------------
-- Supermonad Type Class
-- -----------------------------------------------------------------------------

infixl 1  >>, >>=

-- | See @Control.Supermonad.@'Control.Supermonad.Bind' for details on laws and requirements.
class (Applicative m n p) => Bind m n p where
  type BindCts m n p (a :: *) (b :: *) :: Constraint
  type BindCts m n p a b = ()
  (>>=) :: (BindCts m n p a b) => m a -> (a -> n b) -> p b
  (>>)  :: (BindCts m n p a b) => m a -> n b -> p b
  ma >> mb = ma >>= const mb

instance Bind ((->) r) ((->) r) ((->) r) where
  (>>=) = (P.>>=)
instance Bind Identity Identity Identity where
  (>>=) = (P.>>=)
instance Bind [] [] [] where
  (>>=) = (P.>>=)
instance Bind P.Maybe P.Maybe P.Maybe where
  (>>=) = (P.>>=)
instance Bind P.IO P.IO P.IO where
  (>>=) = (P.>>=)
instance Bind (P.Either e) (P.Either e) (P.Either e) where
  (>>=) = (P.>>=)

instance Bind Mon.First Mon.First Mon.First where
  (>>=) = (P.>>=)
instance Bind Mon.Last Mon.Last Mon.Last where
  (>>=) = (P.>>=)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Bind Mon.Sum Mon.Sum Mon.Sum where
  (>>=) = (P.>>=)
instance Bind Mon.Product Mon.Product Mon.Product where
  (>>=) = (P.>>=)
instance Bind Mon.Dual Mon.Dual Mon.Dual where
  (>>=) = (P.>>=)
#endif
instance (Bind f f f) => Bind (Mon.Alt f) (Mon.Alt f) (Mon.Alt f) where
  type BindCts (Mon.Alt f) (Mon.Alt f) (Mon.Alt f) a b = BindCts f f f a b
  m >>= f = Mon.Alt $ (Mon.getAlt m) >>= (Mon.getAlt . f)

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Bind Semigroup.Min Semigroup.Min Semigroup.Min where
  (>>=) = (P.>>=)
instance Bind Semigroup.Max Semigroup.Max Semigroup.Max where
  (>>=) = (P.>>=)
instance Bind Semigroup.Option Semigroup.Option Semigroup.Option where
  (>>=) = (P.>>=)
instance Bind Semigroup.First Semigroup.First Semigroup.First where
  (>>=) = (P.>>=)
instance Bind Semigroup.Last Semigroup.Last Semigroup.Last where
  (>>=) = (P.>>=)
#endif

instance Bind Proxy.Proxy Proxy.Proxy Proxy.Proxy where
  (>>=) = (P.>>=)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Bind Complex.Complex Complex.Complex Complex.Complex where
  (>>=) = (P.>>=)
instance Bind NonEmpty.NonEmpty NonEmpty.NonEmpty NonEmpty.NonEmpty where
  (>>=) = (P.>>=)
#endif
instance (Bind m1 n1 p1, Bind m2 n2 p2) => Bind (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) where
  type BindCts (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) a b = (BindCts m1 n1 p1 a b, BindCts m2 n2 p2 a b)
  Product.Pair m1 m2 >>= f = Product.Pair (m1 >>= (fstP . f)) (m2 >>= (sndP . f))
    where fstP (Product.Pair a _) = a
          sndP (Product.Pair _ b) = b

instance Bind Read.ReadP Read.ReadP Read.ReadP where
  (>>=) = (P.>>=)
instance Bind Read.ReadPrec Read.ReadPrec Read.ReadPrec where
  (>>=) = (P.>>=)

instance Bind (ST.ST s) (ST.ST s) (ST.ST s) where
  (>>=) = (P.>>=)
instance Bind (STL.ST s) (STL.ST s) (STL.ST s) where
  (>>=) = (P.>>=)
instance (Arrow.ArrowApply a) => Bind (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) where
  (>>=) = (P.>>=)
instance (Bind m n p) => Bind (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) where
  type BindCts (App.WrappedMonad m) (App.WrappedMonad n) (App.WrappedMonad p) a b = BindCts m n p a b
  m >>= f = App.WrapMonad $ (App.unwrapMonad m) >>= (App.unwrapMonad . f)

instance Bind STM.STM STM.STM STM.STM where
  (>>=) = (P.>>=)

-- Constrained Instances -------------------------------------------------------

instance Bind S.Set S.Set S.Set where
  type BindCts S.Set S.Set S.Set a b = Ord b
  s >>= f = S.foldr S.union S.empty $ S.map f s


-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance {- (Bind m n p) => -} Bind (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) where
  type BindCts (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) a b = () -- (BindCts m n p)
  m >>= k = Cont.ContT $ \ c -> Cont.runContT m (\ x -> Cont.runContT (k x) c)
  {-# INLINE (>>=) #-}

instance (Bind m n p, Return n) => Bind (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) where
  type BindCts (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) a b = (BindCts m n p (P.Either e a) (P.Either e b), ReturnCts n (P.Either e b))
  m >>= k = Except.ExceptT $ 
      Except.runExceptT m >>= 
      \ a -> case a of
          P.Left e -> return (P.Left e)
          P.Right x -> Except.runExceptT (k x)
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) where
  type BindCts (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) a b = (BindCts m n p a b)
  m >>= k = Identity.IdentityT $ Identity.runIdentityT m >>= (Identity.runIdentityT . k) 
  {-# INLINE (>>=) #-}

-- Requires undecidable instances.
instance (Bind m n p, Bind n n n, Return n) => Bind (List.ListT m) (List.ListT n) (List.ListT p) where
  type BindCts (List.ListT m) (List.ListT n) (List.ListT p) a b = (BindCts m n p [a] [b], BindCts n n n [b] [[b]], ReturnCts n [[b]], FunctorCts n [[b]] [[b]], FunctorCts n [[b]] [b])
  (>>=) :: forall a b. (BindCts m n p [a] [b], BindCts n n n [b] [[b]], ReturnCts n [[b]], FunctorCts n [[b]] [[b]], FunctorCts n [[b]] [b]) => List.ListT m a -> (a -> List.ListT n b) -> List.ListT p b
  m >>= f = List.ListT $
      List.runListT m >>=
      \ a -> fmap P.concat $ P.foldr k (return []) a
    where
      k :: (BindCts n n n [b] [[b]], FunctorCts n [[b]] [[b]]) => a -> n [[b]] -> n [[b]]
      k a r = List.runListT (f a) >>= \ x -> fmap (x :) r
  {-# INLINE (>>=) #-}

instance (Return n, Bind m n p) => Bind (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) where
  type BindCts (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) a b = (ReturnCts n (P.Maybe b), BindCts m n p (P.Maybe a) (P.Maybe b))
  x >>= f = Maybe.MaybeT $
    Maybe.runMaybeT x >>=
    \v -> case v of
      P.Nothing -> return P.Nothing
      P.Just y  -> Maybe.runMaybeT (f y)
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) where
  type BindCts (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) a b = (BindCts m n p (a, s, w) (b, s, w), FunctorCts n (b, s, w) (b, s, w))
  m >>= k  = RWSL.RWST $ 
    \ r s -> RWSL.runRWST m r s >>=
    \ ~(a, s', w) -> fmap (\ ~(b, s'',w') -> (b, s'', w `P.mappend` w')) $ RWSL.runRWST (k a) r s'
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) where
  type BindCts (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) a b = (BindCts m n p (a, s, w) (b, s, w), FunctorCts n (b, s, w) (b, s, w))
  m >>= k  = RWSS.RWST $ 
    \ r s -> RWSS.runRWST m r s >>=
    \ (a, s', w) -> fmap (\(b, s'',w') -> (b, s'', w `P.mappend` w')) $ RWSS.runRWST (k a) r s'
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) where
  type BindCts (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) a b = (BindCts m n p a b)
  m >>= k  = Reader.ReaderT $ 
      \ r -> Reader.runReaderT m r >>=
      \ a -> Reader.runReaderT (k a) r
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) where
  type BindCts (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) a b = (BindCts m n p (a, s) (b, s))
  m >>= k = StateL.StateT 
          $ \ s -> StateL.runStateT m s >>= 
            \ ~(a, s') -> StateL.runStateT (k a) s'
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) where
  type BindCts (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) a b = (BindCts m n p (a, s) (b, s))
  m >>= k = StateS.StateT 
          $ \ s -> StateS.runStateT m s >>= 
            \ (a, s') -> StateS.runStateT (k a) s'
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) where
  type BindCts (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) a b = (BindCts m n p (a, w) (b, w), FunctorCts n (b, w) (b, w))
  m >>= k  = WriterL.WriterT $
      WriterL.runWriterT m >>=
      \ ~(a, w) -> fmap (\ ~(b, w') -> (b, w `P.mappend` w')) $ WriterL.runWriterT (k a)
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) where
  type BindCts (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) a b = (BindCts m n p (a, w) (b, w), FunctorCts n (b, w) (b, w))
  m >>= k  = WriterS.WriterT $
      WriterS.runWriterT m >>=
      \ (a, w) -> fmap (\ (b, w') -> (b, w `P.mappend` w')) $ WriterS.runWriterT (k a)
  {-# INLINE (>>=) #-}

-- -----------------------------------------------------------------------------
-- Return Type Class
-- -----------------------------------------------------------------------------

-- | See 'Bind' for details on laws and requirements.
class (Functor m) => Return m where
  type ReturnCts m (a :: *) :: Constraint
  type ReturnCts m a = ()
  return :: (ReturnCts m a) => a -> m a

instance Return ((->) r) where
  return = P.return
instance Return Identity where
  return = P.return
instance Return [] where
  return = P.return
instance Return P.Maybe where
  return = P.return
instance Return P.IO where
  return = P.return
instance Return (P.Either e) where
  return = P.return

instance Return Mon.First where
  return = P.return
instance Return Mon.Last where
  return = P.return
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Return Mon.Sum where
  return = P.return
instance Return Mon.Product where
  return = P.return
instance Return Mon.Dual where
  return = P.return
#endif
instance (Return m) => Return (Mon.Alt m) where
  type ReturnCts (Mon.Alt m) a = ReturnCts m a
  return a = Mon.Alt $ return a

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Return Semigroup.Min where
  return = P.return
instance Return Semigroup.Max where
  return = P.return
instance Return Semigroup.Option where
  return = P.return
instance Return Semigroup.First where
  return = P.return
instance Return Semigroup.Last where
  return = P.return
#endif

instance Return Proxy.Proxy where
  return = P.return
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Return Complex.Complex where
  return = P.return
instance Return NonEmpty.NonEmpty where
  return = P.return
#endif
instance (Return m1, Return m2) => Return (Product.Product m1 m2) where
  type ReturnCts (Product.Product m1 m2) a = (ReturnCts m1 a, ReturnCts m2 a)
  return a = Product.Pair (return a) (return a)

instance Return Read.ReadP where
  return = P.return
instance Return Read.ReadPrec where
  return = P.return

instance Return (ST.ST s) where
  return = P.return
instance Return (STL.ST s) where
  return = P.return
instance (Arrow.ArrowApply a) => Return (Arrow.ArrowMonad a) where
  return = P.return
instance (Return m) => Return (App.WrappedMonad m) where
  type ReturnCts (App.WrappedMonad m) a = ReturnCts m a
  return a = App.WrapMonad $ return a

instance Return STM.STM where
  return = P.return

-- Constrained Instances -------------------------------------------------------

instance Return S.Set where
  return = S.singleton

-- "transformers" package instances: -------------------------------------------

-- Continuations are so weird...
instance {- (Return m) => -} Return (Cont.ContT r m) where
  type ReturnCts (Cont.ContT r m) a = () -- ReturnCts m
  return x = Cont.ContT ($ x)
  {-# INLINE return #-}

instance (Return m) => Return (Except.ExceptT e m) where
  type ReturnCts (Except.ExceptT e m) a = ReturnCts m (P.Either e a)
  return = Except.ExceptT . return . P.Right
  {-# INLINE return #-}

instance (Return m) => Return (Identity.IdentityT m) where
  type ReturnCts (Identity.IdentityT m) a = ReturnCts m a
  return = (Identity.IdentityT) . return
  {-# INLINE return #-}

instance (Return m) => Return (List.ListT m) where
  type ReturnCts (List.ListT m) a = ReturnCts m [a]
  return a = List.ListT $ return [a]
  {-# INLINE return #-}

instance (Return m) => Return (Maybe.MaybeT m) where
  type ReturnCts (Maybe.MaybeT m) a = ReturnCts m (P.Maybe a)
  return = Maybe.MaybeT . return . P.Just
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (RWSL.RWST r w s m) where
  type ReturnCts (RWSL.RWST r w s m) a = ReturnCts m (a, s, w)
  return a = RWSL.RWST $ \ _ s -> return (a, s, P.mempty)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (RWSS.RWST r w s m) where
  type ReturnCts (RWSS.RWST r w s m) a = ReturnCts m (a, s, w)
  return a = RWSS.RWST $ \ _ s -> return (a, s, P.mempty)
  {-# INLINE return #-}

instance (Return m) => Return (Reader.ReaderT r m) where
  type ReturnCts (Reader.ReaderT s m) a = ReturnCts m a
  return = Reader.ReaderT . const . return
  {-# INLINE return #-}

instance (Return m) => Return (StateL.StateT s m) where
  type ReturnCts (StateL.StateT s m) a = ReturnCts m (a, s)
  return x = StateL.StateT $ \s -> return (x, s)
  {-# INLINE return #-}

instance (Return m) => Return (StateS.StateT s m) where
  type ReturnCts (StateS.StateT s m) a = ReturnCts m (a, s)
  return x = StateS.StateT $ \s -> return (x, s)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (WriterL.WriterT w m) where
  type ReturnCts (WriterL.WriterT w m) a = ReturnCts m (a, w)
  return a = WriterL.WriterT $ return (a, P.mempty)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (WriterS.WriterT w m) where
  type ReturnCts (WriterS.WriterT w m) a = ReturnCts m (a, w)
  return a = WriterS.WriterT $ return (a, P.mempty)
  {-# INLINE return #-}

-- -----------------------------------------------------------------------------
-- Fail Type Class
-- -----------------------------------------------------------------------------

-- | See 'Bind' for details on laws and requirements.
class Fail m where
  type FailCts m (a :: *) :: Constraint
  type FailCts m a = ()
  fail :: (FailCts m a) => String -> m a

instance Fail ((->) r) where
  fail = P.fail
instance Fail Identity where
  fail = P.fail
instance Fail [] where
  fail = P.fail
instance Fail P.Maybe where
  fail = P.fail
instance Fail P.IO where
  fail = P.fail
instance Fail (P.Either e) where
  fail = P.fail

instance Fail Mon.First where
  fail = P.fail
instance Fail Mon.Last where
  fail = P.fail
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Fail Mon.Sum where
  fail = P.fail
instance Fail Mon.Product where
  fail = P.fail
instance Fail Mon.Dual where
  fail = P.fail
#endif
instance (Fail m) => Fail (Mon.Alt m) where
  type FailCts (Mon.Alt m) a = FailCts m a
  fail a = Mon.Alt $ fail a

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Fail Semigroup.Min where
  fail = P.fail
instance Fail Semigroup.Max where
  fail = P.fail
instance Fail Semigroup.Option where
  fail = P.fail
instance Fail Semigroup.First where
  fail = P.fail
instance Fail Semigroup.Last where
  fail = P.fail
#endif

instance Fail Proxy.Proxy where
  fail = P.fail
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
instance Fail Complex.Complex where
  fail = P.fail
instance Fail NonEmpty.NonEmpty where
  fail = P.fail
#endif
instance (Fail m1, Fail m2) => Fail (Product.Product m1 m2) where
  type FailCts (Product.Product m1 m2) a = (FailCts m1 a, FailCts m2 a)
  fail a = Product.Pair (fail a) (fail a)

instance Fail Read.ReadP where
  fail = P.fail
instance Fail Read.ReadPrec where
  fail = P.fail

instance Fail (ST.ST s) where
  fail = P.fail
instance Fail (STL.ST s) where
  fail = P.fail
instance (Arrow.ArrowApply a) => Fail (Arrow.ArrowMonad a) where
  fail = P.fail
instance (Fail m) => Fail (App.WrappedMonad m) where
  type FailCts (App.WrappedMonad m) a = FailCts m a
  fail a = App.WrapMonad $ fail a

instance Fail STM.STM where
  fail = P.fail

-- Constrained Instances -------------------------------------------------------

instance Fail S.Set where
  fail _ = S.empty

-- "transformers" package instances: -------------------------------------------

instance (Fail m) => Fail (Cont.ContT r m) where
  type FailCts (Cont.ContT r m) a = (FailCts m r)
  fail = (Cont.ContT) . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (Except.ExceptT e m) where
  type FailCts (Except.ExceptT e m) a = (FailCts m (P.Either e a))
  fail = Except.ExceptT . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (Identity.IdentityT m) where
  type FailCts (Identity.IdentityT m) a = (FailCts m a)
  fail msg = Identity.IdentityT $ fail msg
  {-# INLINE fail #-}

instance (Return m) => Fail (List.ListT m) where
  type FailCts (List.ListT m) a = (ReturnCts m [a])
  fail _ = List.ListT $ return []
  {-# INLINE fail #-}

instance (Return m) => Fail (Maybe.MaybeT m) where
  type FailCts (Maybe.MaybeT m) a = (ReturnCts m (P.Maybe a))
  fail _ = Maybe.MaybeT (return P.Nothing)
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (RWSL.RWST r w s m) where
  type FailCts (RWSL.RWST r w s m) a = (FailCts m (a, s, w))
  fail msg = RWSL.RWST $ \ _ _ -> fail msg
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (RWSS.RWST r w s m) where
  type FailCts (RWSS.RWST r w s m) a = (FailCts m (a, s, w))
  fail msg = RWSS.RWST $ \ _ _ -> fail msg
  {-# INLINE fail #-}

instance (Fail m) => Fail (Reader.ReaderT r m) where
  type FailCts (Reader.ReaderT r m) a = (FailCts m a)
  fail = Reader.ReaderT . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (StateL.StateT s m) where
  type FailCts (StateL.StateT s m) a = (FailCts m (a, s))
  fail = StateL.StateT . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (StateS.StateT s m) where
  type FailCts (StateS.StateT s m) a = (FailCts m (a, s))
  fail = StateS.StateT . const . fail
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (WriterL.WriterT w m) where
  type FailCts (WriterL.WriterT w m) a = (FailCts m (a, w))
  fail msg = WriterL.WriterT $ fail msg
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (WriterS.WriterT w m) where
  type FailCts (WriterS.WriterT w m) a = (FailCts m (a, w))
  fail msg = WriterS.WriterT $ fail msg
  {-# INLINE fail #-}

-- -----------------------------------------------------------------------------
-- Convenient type synonyms
-- -----------------------------------------------------------------------------

-- | A short-hand for writing polymorphic standard monad functions.
type family Monad m :: Constraint where
  Monad m = (Bind m m m, Return m, Fail m)



