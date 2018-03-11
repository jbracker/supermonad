
-- | Custom prelude to use if supermonads need to support constrained monads as well. 
module Control.Super.Monad.Constrained.Prelude 
  ( -- * Supermonads
    module Control.Super.Monad.Constrained
    -- ** Replacement functions
  , F.mapM_, F.sequence_, (F.=<<)
    -- ** Traversable replacement functions
  , F.mapM, F.sequence
    -- * Fix rebindable syntax
  , F.ifThenElse
    -- * Prelude functions
  , module Control.Super.Monad.PreludeWithoutMonad
  ) where

import Control.Super.Monad.PreludeWithoutMonad hiding ( Functor(..) )
import Control.Super.Monad.Constrained
import qualified Control.Super.Monad.Constrained.Functions as F

