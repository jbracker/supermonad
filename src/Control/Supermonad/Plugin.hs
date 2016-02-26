-- | Provides the polymonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )

import Control.Supermonad.Plugin.Environment 
  ( SupermonadPluginM, runSupermonadPlugin
  , printMsg )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

-- | The polymonad type checker plugin for GHC.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_clOpts -> Just supermonadPlugin }

-- -----------------------------------------------------------------------------
-- Actual Plugin Code
-- -----------------------------------------------------------------------------

type SupermonadState = ()

supermonadPlugin :: TcPlugin
supermonadPlugin = TcPlugin
  { tcPluginInit  = supermonadInit
  , tcPluginSolve = supermonadSolve
  , tcPluginStop  = supermonadStop
  }

supermonadInit :: TcPluginM SupermonadState
supermonadInit = return ()

supermonadStop :: SupermonadState -> TcPluginM ()
supermonadStop _s = return ()

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

supermonadSolve' :: SupermonadState -> SupermonadPluginM TcPluginResult
supermonadSolve' _s = do
  return noResult
  -- TODO

noResult :: TcPluginResult
noResult = TcPluginOk [] []

