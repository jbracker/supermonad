
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )

import Control.Supermonad.Plugin.Log ( sDocToStr )
import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Dict ( SupermonadDict )
import Control.Supermonad.Plugin.Solving
  ( solveConstraints )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM
  , initSupermonadPlugin, runSupermonadPlugin
  , getWantedConstraints
  , getTypeEqualities, getTyVarEqualities
  , printMsg
  -- , printObj, printConstraints
  )
import Control.Supermonad.Plugin.Constraint 
  ( mkDerivedTypeEqCt, mkDerivedTypeEqCtOfTypes )

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
supermonadSolve _s given derived wanted = do
  res <- runSupermonadPlugin (given ++ derived) wanted initSupermonadPlugin $
    if not $ null wanted then do
      printMsg "Invoke supermonad plugin..."
      supermonadSolve'
      
      tyVarEqs <- getTyVarEqualities
      let tyVarEqCts = fmap (\(baseCt, tv, ty) -> mkDerivedTypeEqCt baseCt tv ty) tyVarEqs
      
      tyEqs <- getTypeEqualities
      let tyEqCts = fmap (\(baseCt, ta, tb) -> mkDerivedTypeEqCtOfTypes baseCt ta tb) tyEqs
      
      return $ TcPluginOk [] $ tyVarEqCts ++ tyEqCts
    else 
      return noResult
  case res of
    Left err -> do
      L.printErr $ sDocToStr err
      return noResult
    Right solution -> return solution

-- | The actual plugin code.
supermonadSolve' :: SupermonadPluginM SupermonadDict ()
supermonadSolve' = do
  --(getWantedConstraints >>= filterM isBindConstraint) >>= (printConstraints . sortConstraintsByLine)
  --(getWantedConstraints >>= filterM isReturnConstraint) >>= (printConstraints . sortConstraintsByLine)
  --getGivenConstraints >>= (printConstraints . sortConstraintsByLine)
  
  solveConstraints =<< getWantedConstraints 
  

noResult :: TcPluginResult
noResult = TcPluginOk [] []







