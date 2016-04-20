
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Data.Maybe ( catMaybes, isNothing )
import Data.List ( nubBy )
import qualified Data.Set as S

import Control.Monad ( forM, forM_, filterM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )

import Control.Supermonad.Plugin.Log ( sDocToStr )
import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Utils 
  ( collectTyVars )
import Control.Supermonad.Plugin.Constraint
  ( DerivedCt, WantedCt
  , mkDerivedTypeEqCt
  , mkDerivedTypeEqCtOfTypes
  , constraintClassTyArgs
  , constraintTopTcVars
  , constraintTopTyCons
  , sortConstraintsByLine )
import Control.Supermonad.Plugin.Solving
  ( solveConstraints )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM, runSupermonadPlugin
  , getIdentityTyCon
  , getReturnClass, getBindClass
  , getWantedConstraints, getGivenConstraints
  , getSupermonadFor
  , getInstEnvs
  , addEvidenceResult
  , addDerivedResults
  , getDerivedResults
  , whenNoResults
  , runTcPlugin
  , printMsg, printObj, printConstraints )
import Control.Supermonad.Plugin.Environment.Lift
  ( isBindConstraint, isReturnConstraint )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

-- | The supermonad type checker plugin for GHC.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_clOpts -> Just supermonadPlugin }

-- -----------------------------------------------------------------------------
-- Actual Plugin Code
-- -----------------------------------------------------------------------------

type SupermonadState = ()

-- | The type checker plugin.
supermonadPlugin :: TcPlugin
supermonadPlugin = TcPlugin
  { tcPluginInit  = supermonadInit
  , tcPluginSolve = supermonadSolve
  , tcPluginStop  = supermonadStop
  }

-- | No initialization needs takes place.
supermonadInit :: TcPluginM SupermonadState
supermonadInit = return ()

-- | No clean up needs to take place.
supermonadStop :: SupermonadState -> TcPluginM ()
supermonadStop _s = return ()

-- | The plugin code wrapper. Handles execution of the monad stack.
supermonadSolve :: SupermonadState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
supermonadSolve s given derived wanted = do
  res <- runSupermonadPlugin (given ++ derived) wanted $
    if not $ null wanted then do
      printMsg "Invoke supermonad plugin..."
      supermonadSolve' s
    else return ()
  case res of
    Left err -> do
      L.printErr $ sDocToStr err
      return noResult
    Right solution -> return solution

-- | The actual plugin code.
supermonadSolve' :: SupermonadState -> SupermonadPluginM ()
supermonadSolve' _s = do
  --(getWantedConstraints >>= filterM isBindConstraint) >>= (printConstraints . sortConstraintsByLine)
  --(getWantedConstraints >>= filterM isReturnConstraint) >>= (printConstraints . sortConstraintsByLine)
  --getGivenConstraints >>= (printConstraints . sortConstraintsByLine)
  
  solveConstraints =<< getWantedConstraints 
  

noResult :: TcPluginResult
noResult = TcPluginOk [] []







