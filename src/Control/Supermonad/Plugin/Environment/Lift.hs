
-- | Provides versions of functions written for 'TcPluginM'
--   that are lifted into 'SupermonadPluginM'.
module Control.Supermonad.Plugin.Environment.Lift
  (
  -- * From 'Control.Supermonad.Plugin.Evidence'
    produceEvidenceForCt
  , produceEvidenceFor
  -- * From 'Control.Supermonad.Plugin.Constraint'
  , isBindConstraint, isReturnConstraint
  -- * From 'Control.Supermonad.Plugin.Utils'
  , partiallyApplyTyCons ) where

import TcRnTypes ( Ct )
import TcEvidence ( EvTerm )
import Outputable ( SDoc )
import Type ( Type, TyVar )
import TyCon ( TyCon )
import InstEnv ( ClsInst )

import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM
  , runTcPlugin
  , getBindClass, getReturnClass
  --, getSupermonadModule
  --, getIdentityTyCon --, getIdentityModule
  , getGivenConstraints --, getWantedConstraints
  --, setWantedConstraints
  --, getInstEnvs
  --, getBindInstances 
  )

import qualified Control.Supermonad.Plugin.Utils as U
import qualified Control.Supermonad.Plugin.Evidence as E
import Control.Supermonad.Plugin.Constraint ( isClassConstraint )

-- | See 'E.produceEvidenceForCt'.
produceEvidenceForCt :: Ct -> SupermonadPluginM (Either SDoc EvTerm)
produceEvidenceForCt ct = do
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceForCt givenCts ct

-- | See 'E.produceEvidenceFor'.
produceEvidenceFor :: ClsInst -> [Type] -> SupermonadPluginM (Either SDoc EvTerm)
produceEvidenceFor inst instArgs = do
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceFor givenCts inst instArgs

-- | See 'getBindClass' and 'isClassConstraint'.
isBindConstraint :: Ct -> SupermonadPluginM Bool
isBindConstraint ct = do
  bindCls <- getBindClass
  return $ isClassConstraint bindCls ct

-- | See 'getReturnClass' and 'isClassConstraint'.
isReturnConstraint :: Ct -> SupermonadPluginM Bool
isReturnConstraint ct = do
  returnCls <- getReturnClass
  return $ isClassConstraint returnCls ct

-- | See 'U.partiallyApplyTyCons'.
partiallyApplyTyCons :: [(TyVar, Either TyCon TyVar)] -> SupermonadPluginM (Either SDoc [(TyVar, Type, [TyVar])])
partiallyApplyTyCons = runTcPlugin . U.partiallyApplyTyCons

