-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Data.Maybe ( catMaybes )

import Control.Monad ( forM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import Type ( getTyVar, mkTyConTy )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )

import Control.Supermonad.Plugin.Utils ( isAmbiguousType )
import Control.Supermonad.Plugin.Constraint
  ( mkDerivedTypeEqCt
  , constraintClassTyArgs )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM, runSupermonadPlugin
  , getWantedReturnConstraints
  , getIdentityTyCon
  , printMsg )

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
    else return noResult
  return $ case res of
    Left _err -> noResult
    Right solution -> solution

-- | The actual plugin code.
supermonadSolve' :: SupermonadState -> SupermonadPluginM TcPluginResult
supermonadSolve' _s = do
  -- Get information from the environment
  identityTC <- getIdentityTyCon
  returnCts <- getWantedReturnConstraints

  -- Default all ambiguous type variables in 'Return' constraints
  -- to 'Identity'.
  derivedReturnCts <- forM returnCts $ \returnCt ->
    case constraintClassTyArgs returnCt of
      Just [t] -> if isAmbiguousType t
        then do
          let ambTv = getTyVar "Type is not a TyVar" t
          return $ Just $ mkDerivedTypeEqCt returnCt ambTv (mkTyConTy identityTC)
        else return Nothing
      _ -> return Nothing

  -- Return the derived constraints and evidence for selected instances.
  return $ TcPluginOk [] (catMaybes derivedReturnCts)

noResult :: TcPluginResult
noResult = TcPluginOk [] []
