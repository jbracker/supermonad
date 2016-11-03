
{-# LANGUAGE NoImplicitPrelude #-}

-- | A replacement of the standard "Prelude" for supermonads. Should provide
--   all of the functions also provided in the original prelude without
--   the functions related specifically to 'P.Monad's. The functions related
--   to 'P.Monad's are replaced with their supermonad counterparts.
--
--   A replacement for the functions in "Control.Monad" can be found 
--   in "Control.Super.Monad.Functions".
module Control.Super.Monad.Prelude
  ( -- * Supermonads
    module Control.Super.Monad
    -- ** Replacement functions
  , F.mapM_, F.sequence_, (F.=<<)
    -- ** Traversable replacement functions
  , F.mapM, F.sequence
    -- * Fix rebindable syntax
  , F.ifThenElse
    -- * Prelude functions
  , module Control.Super.Monad.PreludeWithoutMonad
  ) where


import Control.Super.Monad
import Control.Super.Monad.PreludeWithoutMonad
import qualified Control.Super.Monad.Functions as F
