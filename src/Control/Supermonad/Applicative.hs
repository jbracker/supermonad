
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-} -- For 'Applicative' class.
{-# LANGUAGE TypeFamilies          #-} -- For 'Applicative' class.
{-# LANGUAGE ConstraintKinds       #-} -- For 'Applicative' class.

module Control.Supermonad.Applicative
  ( Applicative(..)
  , pure
  ) where

import Prelude
  ( Maybe(..), Either(..)
  , Functor(..)
  , (.), ($), const, id
  )
import qualified Prelude as P

import Control.Supermonad

import GHC.Exts ( Constraint )

-- To define standard instances:
import qualified Data.Monoid as Mon ( First, Last, Sum, Product, Dual, Alt(..) )
import qualified Data.Proxy as Proxy ( Proxy )
import qualified Data.Complex as Complex ( Complex )
import qualified Data.Functor.Product as Product ( Product(..) )
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import qualified Data.Semigroup as Semigroup ( Min, Max, Option, First, Last )
import qualified Data.List.NonEmpty as NonEmpty ( NonEmpty )
#endif

import qualified Control.Arrow as Arrow ( ArrowMonad, Arrow )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )

-- To define "transformers" instances:
import qualified Control.Monad.Trans.Cont     as Cont     ( ContT(..) )
import qualified Control.Monad.Trans.Except   as Except   ( ExceptT(..) )
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

-- -----------------------------------------------------------------------------
-- Super-Applicative Type Class
-- -----------------------------------------------------------------------------

-- | TODO
class (Functor m, Functor n, Functor p) => Applicative m n p where
  type ApplicativeCts m n p :: Constraint
  type ApplicativeCts m n p = ()
  
  (<*>) :: (ApplicativeCts m n p) => m (a -> b) -> n a -> p b
  
  (*>) :: (ApplicativeCts m n p) => m a -> n b -> p b
  ma *> nb = (id <$ ma) <*> nb
  (<*) :: (ApplicativeCts m n p) => m a -> n b -> p a
  ma <* nb = fmap const ma <*> nb

-- | 'pure' is defined in terms of return.
pure :: (Return f, ReturnCts f) => a -> f a
pure = return

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
  type ApplicativeCts (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) = ApplicativeCts m n p
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
  type ApplicativeCts (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) = (ApplicativeCts m1 n1 p1, ApplicativeCts m2 n2 p2)
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

instance (Arrow.Arrow a) => Applicative (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) (Arrow.ArrowMonad a) where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)
-- | TODO / FIXME: The wrapped monad instances for 'Functor' and 'P.Monad' are both 
--   based on @m@ being a monad, although the functor instance should only be 
--   dependend on @m@ being a functor (as can be seen below). This can only be
--   fixed by either giving a custom version of 'App.WrappedMonad' here or by fixing
--   the version of 'App.WrappedMonad' in base.
--   Once this issue is fixed we can replace the 'P.Monad' constraint
--   with a 'Functor' constraint.
--   
--   > instance (Functor m) => Functor (App.WrappedMonad m) where
--   >   fmap f m = App.WrapMonad $ fmap (App.unwrapMonad m) f
--   
instance (Applicative m m m, P.Monad m) => Applicative (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) where
  type ApplicativeCts (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) = ApplicativeCts m m m
  mf <*> na = App.WrapMonad $ (App.unwrapMonad mf) <*> (App.unwrapMonad na)
  mf *> na = App.WrapMonad $ (App.unwrapMonad mf) *> (App.unwrapMonad na)
  mf <* na = App.WrapMonad $ (App.unwrapMonad mf) <* (App.unwrapMonad na)

instance Applicative STM.STM STM.STM STM.STM where
  (<*>) = (P.<*>)
  (<*)  = (P.<*)
  (*>)  = (P.*>)

-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance Applicative (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) where
  type ApplicativeCts (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) = ()
  f <*> a = Cont.ContT $ \ c -> Cont.runContT f $ \ g -> Cont.runContT a (c . g)
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) where
  type ApplicativeCts (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) = (ApplicativeCts m n p)
  Except.ExceptT f <*> Except.ExceptT v = Except.ExceptT $ fmap (<*>) f <*> v
  {-# INLINEABLE (<*>) #-}

instance (Applicative m n p) => Applicative (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) where
  type ApplicativeCts (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) = (ApplicativeCts m n p)
  Identity.IdentityT m <*> Identity.IdentityT k = Identity.IdentityT $ m <*> k
  {-# INLINE (<*>) #-}

-- Requires undecidable instances.
instance (Applicative m n p) => Applicative (List.ListT m) (List.ListT n) (List.ListT p) where
  type ApplicativeCts (List.ListT m) (List.ListT n) (List.ListT p) = (ApplicativeCts m n p)
  List.ListT fs <*> List.ListT as = List.ListT $ fmap (<*>) fs <*> as
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) where
  type ApplicativeCts (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) = (ApplicativeCts m n p)
  Maybe.MaybeT f <*> Maybe.MaybeT x = Maybe.MaybeT $ fmap (<*>) f <*> x
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Bind m n p) => Applicative (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) where
  type ApplicativeCts (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) = (BindCts m n p)
  RWSL.RWST mf <*> RWSL.RWST ma  = RWSL.RWST $ \r s -> mf r s >>= \ ~(f, s', w) -> fmap (\ ~(a, s'', w') -> (f a, s'', P.mappend w w')) (ma r s')
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Bind m n p) => Applicative (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) where
  type ApplicativeCts (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) = (BindCts m n p)
  RWSS.RWST mf <*> RWSS.RWST ma = RWSS.RWST $ \r s -> mf r s >>= \ (f, s', w) -> fmap (\ (a, s'', w') -> (f a, s'', P.mappend w w')) (ma r s')
  {-# INLINE (<*>) #-}

instance (Applicative m n p) => Applicative (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) where
  type ApplicativeCts (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) = (ApplicativeCts m n p)
  Reader.ReaderT mf <*> Reader.ReaderT ma  = Reader.ReaderT $ \r -> mf r <*> ma r
  {-# INLINE (<*>) #-}

instance (Bind m n p) => Applicative (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) where
  type ApplicativeCts (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) = (BindCts m n p)
  StateL.StateT mf <*> StateL.StateT ma = StateL.StateT $ \s -> mf s >>= \ ~(f, s') -> fmap (\ ~(a, s'') -> (f a, s'')) (ma s')
  {-# INLINE (<*>) #-}

instance (Bind m n p) => Applicative (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) where
  type ApplicativeCts (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) = (BindCts m n p)
  StateS.StateT mf <*> StateS.StateT ma = StateS.StateT $ \s -> mf s >>= \ (f, s') -> fmap (\ (a, s'') -> (f a, s'')) (ma s')
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Applicative m n p) => Applicative (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) where
  type ApplicativeCts (WriterL.WriterT s m) (WriterL.WriterT s n) (WriterL.WriterT s p) = (ApplicativeCts m n p)
  WriterL.WriterT mf <*> WriterL.WriterT ma = WriterL.WriterT $ fmap (\ ~(f, w) ~(a, w') -> (f a, P.mappend w w')) mf <*> ma
  {-# INLINE (<*>) #-}

instance (P.Monoid w, Applicative m n p) => Applicative (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) where
  type ApplicativeCts (WriterS.WriterT s m) (WriterS.WriterT s n) (WriterS.WriterT s p) = (ApplicativeCts m n p)
  WriterS.WriterT mf <*> WriterS.WriterT ma = WriterS.WriterT $ fmap (\ (f, w) (a, w') -> (f a, P.mappend w w')) mf <*> ma
  {-# INLINE (<*>) #-}




