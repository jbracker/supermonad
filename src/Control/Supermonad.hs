
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

-- | Representation of supermonads in Haskell.
module Control.Supermonad
  ( -- * Supermonads
    Bind(..), Return(..), Fail(..)
    -- * Super-Applicatives
  , Applicative(..), pure
    -- * Conveniences
  , Monad
  ) where

import Prelude
  ( String
  , Maybe(..), Either(..)
  , Functor(..)
  , (.), ($), const, id
  )
import qualified Prelude as P

import Data.Functor.Identity ( Identity )

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

import qualified Control.Arrow as Arrow ( ArrowMonad, ArrowApply, Arrow )
import qualified Control.Applicative as App ( WrappedMonad(..) )
import qualified Control.Monad.ST as ST ( ST )
import qualified Control.Monad.ST.Lazy as STL ( ST )

import qualified Text.ParserCombinators.ReadP as Read ( ReadP )
import qualified Text.ParserCombinators.ReadPrec as Read ( ReadPrec )

import qualified GHC.Conc as STM ( STM )

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


-- -----------------------------------------------------------------------------
-- Supermonad Type Class - Bind
-- -----------------------------------------------------------------------------

-- | Representation of bind operations for supermonads.
--   A proper supermonad consists of an instance 
--   for 'Bind', 'Return' and optionally 'Fail'.
--   
--   The instances are required to follow a certain scheme.
--   If the type constructor of your supermonad is @M@ there
--   may only be exactly one 'Bind' and one 'Return' instance 
--   that look as follows:
--   
-- > instance Bind (M ...) (M ...) (M ...) where
-- >   ...
-- > instance Return (M ...) where
-- >   ...
--   
--   This is enforced by the plugin. A compilation error will
--   result from either instance missing or multiple instances
--   for @M@.
--   
--   For supermonads we expect the usual monad laws to hold:
--   
--   * @'return' a '>>=' k  =  k a@
--   * @m '>>=' 'return'  =  m@
--   * @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@
--   * @'fmap' f xs  =  xs '>>=' 'return' . f@
--   
class (Applicative m n p) => Bind m n p where
  type BindCts m n p :: Constraint
  type BindCts m n p = ()
  (>>=) :: (BindCts m n p) => m a -> (a -> n b) -> p b
  (>>)  :: (BindCts m n p) => m a -> n b -> p b
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
instance (Bind m n p) => Bind (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) where
  type BindCts (Mon.Alt m) (Mon.Alt n) (Mon.Alt p) = BindCts m n p
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
  type BindCts (Product.Product m1 m2) (Product.Product n1 n2) (Product.Product p1 p2) = (BindCts m1 n1 p1, BindCts m2 n2 p2)
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
instance (Bind m m m, P.Monad m) => Bind (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) where
  type BindCts (App.WrappedMonad m) (App.WrappedMonad m) (App.WrappedMonad m) = BindCts m m m
  m >>= f = App.WrapMonad $ (App.unwrapMonad m) >>= (App.unwrapMonad . f)

instance Bind STM.STM STM.STM STM.STM where
  (>>=) = (P.>>=)

-- "transformers" package instances: -------------------------------------------

-- Continuations are so wierd...
-- | TODO / FIXME: Still need to figure out how and if we can generalize the continuation implementation.
instance {- (Bind m n p) => -} Bind (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) where
  type BindCts (Cont.ContT r m) (Cont.ContT r m) (Cont.ContT r m) = () -- (BindCts m n p)
  m >>= k = Cont.ContT $ \ c -> Cont.runContT m (\ x -> Cont.runContT (k x) c)
  {-# INLINE (>>=) #-}

instance (Bind m n p, Return n) => Bind (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) where
  type BindCts (Except.ExceptT e m) (Except.ExceptT e n) (Except.ExceptT e p) = (BindCts m n p, ReturnCts n)
  m >>= k = Except.ExceptT $ 
      Except.runExceptT m >>= 
      \ a -> case a of
          Left e -> return (Left e)
          Right x -> Except.runExceptT (k x)
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) where
  type BindCts (Identity.IdentityT m) (Identity.IdentityT n) (Identity.IdentityT p) = (BindCts m n p)
  m >>= k = Identity.IdentityT $ Identity.runIdentityT m >>= (Identity.runIdentityT . k) 
  {-# INLINE (>>=) #-}

-- Requires undecidable instances.
instance (Bind m n p, Bind n n n, Return n) => Bind (List.ListT m) (List.ListT n) (List.ListT p) where
  type BindCts (List.ListT m) (List.ListT n) (List.ListT p) = (BindCts m n p, BindCts n n n, ReturnCts n)
  (>>=) :: forall a b. (BindCts m n p, BindCts n n n, ReturnCts n) => List.ListT m a -> (a -> List.ListT n b) -> List.ListT p b
  m >>= f = List.ListT $
      List.runListT m >>=
      \ a -> fmap P.concat $ P.foldr k (return []) a
    where
      k :: (BindCts n n n) => a -> n [[b]] -> n [[b]]
      k a r = List.runListT (f a) >>= \ x -> fmap (x :) r
  {-# INLINE (>>=) #-}

instance (Return n, Bind m n p) => Bind (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) where
  type BindCts (Maybe.MaybeT m) (Maybe.MaybeT n) (Maybe.MaybeT p) = (ReturnCts n, BindCts m n p)
  x >>= f = Maybe.MaybeT $
    Maybe.runMaybeT x >>=
    \v -> case v of
      Nothing -> return Nothing
      Just y  -> Maybe.runMaybeT (f y)
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) where
  type BindCts (RWSL.RWST r w s m) (RWSL.RWST r w s n) (RWSL.RWST r w s p) = (BindCts m n p)
  m >>= k  = RWSL.RWST $ 
    \ r s -> RWSL.runRWST m r s >>=
    \ ~(a, s', w) -> fmap (\ ~(b, s'',w') -> (b, s'', w `P.mappend` w')) $ RWSL.runRWST (k a) r s'
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) where
  type BindCts (RWSS.RWST r w s m) (RWSS.RWST r w s n) (RWSS.RWST r w s p) = (BindCts m n p)
  m >>= k  = RWSS.RWST $ 
    \ r s -> RWSS.runRWST m r s >>=
    \ (a, s', w) -> fmap (\(b, s'',w') -> (b, s'', w `P.mappend` w')) $ RWSS.runRWST (k a) r s'
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) where
  type BindCts (Reader.ReaderT r m) (Reader.ReaderT r n) (Reader.ReaderT r p) = (BindCts m n p)
  m >>= k  = Reader.ReaderT $ 
      \ r -> Reader.runReaderT m r >>=
      \ a -> Reader.runReaderT (k a) r
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) where
  type BindCts (StateL.StateT s m) (StateL.StateT s n) (StateL.StateT s p) = (BindCts m n p)
  m >>= k = StateL.StateT 
          $ \ s -> StateL.runStateT m s >>= 
            \ ~(a, s') -> StateL.runStateT (k a) s'
  {-# INLINE (>>=) #-}

instance (Bind m n p) => Bind (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) where
  type BindCts (StateS.StateT s m) (StateS.StateT s n) (StateS.StateT s p) = (BindCts m n p)
  m >>= k = StateS.StateT 
          $ \ s -> StateS.runStateT m s >>= 
            \ (a, s') -> StateS.runStateT (k a) s'
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (WriterL.WriterT w m) (WriterL.WriterT w n) (WriterL.WriterT w p) where
  type BindCts (WriterL.WriterT s m) (WriterL.WriterT s n) (WriterL.WriterT s p) = (BindCts m n p)
  m >>= k  = WriterL.WriterT $
      WriterL.runWriterT m >>=
      \ ~(a, w) -> fmap (\ ~(b, w') -> (b, w `P.mappend` w')) $ WriterL.runWriterT (k a)
  {-# INLINE (>>=) #-}

instance (P.Monoid w, Bind m n p) => Bind (WriterS.WriterT w m) (WriterS.WriterT w n) (WriterS.WriterT w p) where
  type BindCts (WriterS.WriterT s m) (WriterS.WriterT s n) (WriterS.WriterT s p) = (BindCts m n p)
  m >>= k  = WriterS.WriterT $
      WriterS.runWriterT m >>=
      \ (a, w) -> fmap (\ (b, w') -> (b, w `P.mappend` w')) $ WriterS.runWriterT (k a)
  {-# INLINE (>>=) #-}

-- -----------------------------------------------------------------------------
-- Return Type Class
-- -----------------------------------------------------------------------------

-- | See 'Bind' for details on laws and requirements.
class (Functor m) => Return m where
  type ReturnCts m :: Constraint
  type ReturnCts m = ()
  return :: (ReturnCts m) => a -> m a

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
instance (Return a) => Return (Mon.Alt a) where
  type ReturnCts (Mon.Alt a) = ReturnCts a
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
  type ReturnCts (Product.Product m1 m2) = (ReturnCts m1, ReturnCts m2)
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
-- | TODO / FIXME: This has the same issue as the 'Bind' instance for 'App.WrappedMonad'.
instance (Return m, P.Monad m) => Return (App.WrappedMonad m) where
  type ReturnCts (App.WrappedMonad m) = ReturnCts m
  return a = App.WrapMonad $ return a

instance Return STM.STM where
  return = P.return
  
-- "transformers" package instances: -------------------------------------------

-- Continuations are so weird...
instance {- (Return m) => -} Return (Cont.ContT r m) where
  type ReturnCts (Cont.ContT r m) = () -- ReturnCts m
  return x = Cont.ContT ($ x)
  {-# INLINE return #-}

instance (Return m) => Return (Except.ExceptT e m) where
  type ReturnCts (Except.ExceptT e m) = ReturnCts m
  return = Except.ExceptT . return . Right
  {-# INLINE return #-}

instance (Return m) => Return (Identity.IdentityT m) where
  type ReturnCts (Identity.IdentityT m) = ReturnCts m
  return = (Identity.IdentityT) . return
  {-# INLINE return #-}

instance (Return m) => Return (List.ListT m) where
  type ReturnCts (List.ListT m) = ReturnCts m
  return a = List.ListT $ return [a]
  {-# INLINE return #-}

instance (Return m) => Return (Maybe.MaybeT m) where
  type ReturnCts (Maybe.MaybeT m) = ReturnCts m
  return = Maybe.MaybeT . return . Just
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (RWSL.RWST r w s m) where
  type ReturnCts (RWSL.RWST r w s m) = ReturnCts m
  return a = RWSL.RWST $ \ _ s -> return (a, s, P.mempty)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (RWSS.RWST r w s m) where
  type ReturnCts (RWSS.RWST r w s m) = ReturnCts m
  return a = RWSS.RWST $ \ _ s -> return (a, s, P.mempty)
  {-# INLINE return #-}

instance (Return m) => Return (Reader.ReaderT r m) where
  type ReturnCts (Reader.ReaderT s m) = ReturnCts m
  return = Reader.ReaderT . const . return
  {-# INLINE return #-}

instance (Return m) => Return (StateL.StateT s m) where
  type ReturnCts (StateL.StateT s m) = ReturnCts m
  return x = StateL.StateT $ \s -> return (x, s)
  {-# INLINE return #-}

instance (Return m) => Return (StateS.StateT s m) where
  type ReturnCts (StateS.StateT s m) = ReturnCts m
  return x = StateS.StateT $ \s -> return (x, s)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (WriterL.WriterT w m) where
  type ReturnCts (WriterL.WriterT w m) = ReturnCts m
  return a = WriterL.WriterT $ return (a, P.mempty)
  {-# INLINE return #-}

instance (P.Monoid w, Return m) => Return (WriterS.WriterT w m) where
  type ReturnCts (WriterS.WriterT w m) = ReturnCts m
  return a = WriterS.WriterT $ return (a, P.mempty)
  {-# INLINE return #-}

-- -----------------------------------------------------------------------------
-- Fail Type Class
-- -----------------------------------------------------------------------------

-- | See 'Bind' for details on laws and requirements.
class Fail m where
  type FailCts m :: Constraint
  type FailCts m = ()
  fail :: (FailCts m) => String -> m a

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
instance (Fail a) => Fail (Mon.Alt a) where
  type FailCts (Mon.Alt a) = (FailCts a)
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
  type FailCts (Product.Product m1 m2) = (FailCts m1, FailCts m2)
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
-- | TODO / FIXME: This has the same issue as the 'Bind' instance for 'App.WrappedMonad'.
instance (Fail m, P.Monad m) => Fail (App.WrappedMonad m) where
  type FailCts (App.WrappedMonad m) = (FailCts m)
  fail a = App.WrapMonad $ fail a

instance Fail STM.STM where
  fail = P.fail

-- "transformers" package instances: -------------------------------------------

instance (Fail m) => Fail (Cont.ContT r m) where
  type FailCts (Cont.ContT r m) = (FailCts m)
  fail = (Cont.ContT) . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (Except.ExceptT e m) where
  type FailCts (Except.ExceptT e m) = (FailCts m)
  fail = Except.ExceptT . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (Identity.IdentityT m) where
  type FailCts (Identity.IdentityT m) = (FailCts m)
  fail msg = Identity.IdentityT $ fail msg
  {-# INLINE fail #-}

instance (Return m) => Fail (List.ListT m) where
  type FailCts (List.ListT m) = (ReturnCts m)
  fail _ = List.ListT $ return []
  {-# INLINE fail #-}

instance (Return m) => Fail (Maybe.MaybeT m) where
  type FailCts (Maybe.MaybeT m) = (ReturnCts m)
  fail _ = Maybe.MaybeT (return Nothing)
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (RWSL.RWST r w s m) where
  type FailCts (RWSL.RWST r w s m) = (FailCts m)
  fail msg = RWSL.RWST $ \ _ _ -> fail msg
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (RWSS.RWST r w s m) where
  type FailCts (RWSS.RWST r w s m) = (FailCts m)
  fail msg = RWSS.RWST $ \ _ _ -> fail msg
  {-# INLINE fail #-}

instance (Fail m) => Fail (Reader.ReaderT r m) where
  type FailCts (Reader.ReaderT r m) = (FailCts m)
  fail = Reader.ReaderT . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (StateL.StateT s m) where
  type FailCts (StateL.StateT s m) = (FailCts m)
  fail = StateL.StateT . const . fail
  {-# INLINE fail #-}

instance (Fail m) => Fail (StateS.StateT s m) where
  type FailCts (StateS.StateT s m) = (FailCts m)
  fail = StateS.StateT . const . fail
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (WriterL.WriterT w m) where
  type FailCts (WriterL.WriterT w m) = (FailCts m)
  fail msg = WriterL.WriterT $ fail msg
  {-# INLINE fail #-}

instance (P.Monoid w, Fail m) => Fail (WriterS.WriterT w m) where
  type FailCts (WriterS.WriterT w m) = (FailCts m)
  fail msg = WriterS.WriterT $ fail msg
  {-# INLINE fail #-}
  
-- -----------------------------------------------------------------------------
-- Convenient type synonyms
-- -----------------------------------------------------------------------------

-- | A short-hand for writing polymorphic standard monad functions.
type family Monad m :: Constraint where
  Monad m = (Bind m m m, Return m, Fail m)



