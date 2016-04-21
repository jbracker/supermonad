
{-# LANGUAGE NoImplicitPrelude #-}

-- | A replacement of the standard "Prelude" for supermonads. Should provide
--   all of the functions also provided in the original prelude without
--   the functions related specifically to 'P.Monad's. The functions related
--   to 'P.Monad's are replaced with their supermonad counterparts.
--
--   A replacement for the functions in "Control.Monad" can be found 
--   in "Control.Supermonad.Functions".
module Control.Supermonad.Prelude
  ( -- * Supermonads
    module Control.Supermonad
    -- ** Replacement functions
  , F.mapM_, F.sequence_, (F.=<<)
    -- ** Traversable replacement functions
  , F.mapM, F.sequence
    -- * Fix rebindable syntax
  , F.ifThenElse
    -- * Prelude functions
  , module Control.Supermonad.PreludeWithoutMonad
  ) where


import Control.Supermonad
import Control.Supermonad.PreludeWithoutMonad
import qualified Control.Supermonad.Functions as F
