
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-} -- For 'Applicative' class.
{-# LANGUAGE TypeFamilies          #-} -- For 'Applicative' class.
{-# LANGUAGE ConstraintKinds       #-} -- For 'Applicative' class.

module Control.Supermonad.Applicative
  ( 
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




