
{-
  This module does nothing else them to import several standard library 
  functions from the different supermonad modules to ensure they are all 
  defined.
  
  This serves as a list of which functions are still missing by looking
  at those that are currently commented out.
  
  The main purpose of this file is to get an overview of how in sync 
  the constrained and unconstrained versions are.
-}

-- There will be many of these...
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import qualified Prelude as P

-- Base supermonad modules -----------------------------------------------------
import Control.Super.Monad 
  ( Bind( (>>=), (>>), BindCts )
  , Return( return, ReturnCts )
  , Fail( fail, FailCts )
  , Functor( fmap, (<$) )
  , Applicative( (<*>), (*>), (<*), ApplicativeCts )
  , Monad
  , pure
  )
import Control.Super.Monad.Constrained
  ( Bind( (>>=), (>>), BindCts )
  , Return( return, ReturnCts )
  , Fail( fail, FailCts )
  , Functor( fmap, (<$), FunctorCts ) -- DIFF: FunctorCts
  , Applicative( (<*>), (*>), (<*), ApplicativeCts )
  , Monad
  , pure
  )

-- Base superarrow modules -----------------------------------------------------
{-
import Control.Super.Arrow
  ( ArrowArr( arr, ArrowArrCts )
  , ArrowSequence( (>>>), (<<<), ArrowSequenceCts )
  , ArrowSelect( first, second, ArrowSelectCts ) -- DIFF: ArrowSelectCts
  , ArrowParallel( (***), ArrowParallelCts )
  , ArrowFanOut( (&&&), ArrowFanOutCts )
  )
import Control.Super.Arrow.Constrained
  ( ArrowArr( arr, ArrowArrCts )
  , ArrowSequence( (>>>), (<<<), ArrowSequenceCts )
  , ArrowSelect( first, second, ArrowSelectFstCts, ArrowSelectSndCts ) -- DIFF: ArrowSelectFstCts, ArrowSelectSndCts
  , ArrowParallel( (***), ArrowParallelCts )
  , ArrowFanOut( (&&&), ArrowFanOutCts )
  )
-}
-- Base functor modules --------------------------------------------------------
import Data.Functor
  ( Functor( fmap, (<$) )
  )
import Control.Super.Monad.Constrained.Functor
  ( Functor( fmap, (<$), FunctorCts ) -- DIFF: FunctorCts
  )

-- Base supermonad standard library module -------------------------------------
import Control.Super.Monad.Functions
  ( mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  , (=<<), (>=>), (<=<)
  , forever, void, voidM
  , join
  -- , msum, mfilter -- FIXME: Requires an alternative of 'MonadPlus'.
  , filterM
  , mapAndUnzipM
  , zipWithM, zipWithM_
  , foldM, foldM_
  , replicateM, replicateM_
  -- , guard -- FIXME: Requires an alternative of 'Alternative'
  , when, unless
  , liftM, liftM', liftM2, liftM3
  -- , liftM4, liftM5 -- TODO
  , ap
  , (<$!>), (<$>)
  , ifThenElse
  
  , liftA3, liftA2, liftA
  , voidA
  , (<**>)
  , mapA, mapA_
  , forA, forA_
  , filterA
  , sequenceA, sequenceA_
  , traverse
  , zipWithA, zipWithA_
  , mapAndUnzipA
  , replicateA, replicateA_
  , whenA, unlessA
  )
import Control.Super.Monad.Constrained.Functions
  ( mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  , (=<<), (>=>), (<=<)
  , forever, void, voidM
  , join
  -- , msum, mfilter -- FIXME: Requires an alternative of 'MonadPlus'.
  , filterM
  , mapAndUnzipM
  , zipWithM, zipWithM_
  , foldM, foldM_
  , replicateM, replicateM_
  -- , guard -- FIXME: Requires an alternative of 'Alternative'
  , when, unless
  , liftM, liftM', liftM2, liftM3
  -- , liftM4, liftM5 -- TODO
  , ap
  , (<$!>), (<$>)
  , ifThenElse
  
  , liftA3, liftA2, liftA
  , voidA
  , (<**>)
  , mapA, mapA_
  , forA, forA_
  , filterA
  , sequenceA, sequenceA_
  , traverse
  , zipWithA, zipWithA_
  , mapAndUnzipA
  , replicateA, replicateA_
  , whenA, unlessA
  )

main :: P.IO ()
main = P.return ()
