
-- | Provides versions of functions written for 'TcPluginM'
--   that are lifted into 'SupermonadPluginM'.
module Control.Supermonad.Plugin.Environment.Lift
  (
  -- * From 'Control.Supermonad.Plugin.Evidence'
    produceEvidenceForCt
  , produceEvidenceFor
  , isPotentiallyInstantiatedCt
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
  , getIdentityTyCon --, getIdentityModule
  , getGivenConstraints --, getWantedConstraints
  --, setWantedConstraints
  , getBindApplyInstance, getBindFunctorInstance
  --, getInstEnvs
  --, getBindInstances 
  )

import qualified Control.Supermonad.Plugin.Utils as U
import qualified Control.Supermonad.Plugin.Evidence as E
import Control.Supermonad.Plugin.Constraint ( isClassConstraint )

-- | See 'E.produceEvidenceForCt'.
produceEvidenceForCt :: Ct -> SupermonadPluginM (Either SDoc EvTerm)
produceEvidenceForCt ct = do
  bindCls <- getBindClass
  instFunctor <- getBindFunctorInstance
  instApply <- getBindApplyInstance
  idTyCon <- getIdentityTyCon
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceForCt bindCls instFunctor instApply idTyCon givenCts ct

-- | See 'E.produceEvidenceFor'.
produceEvidenceFor :: ClsInst -> [Type] -> SupermonadPluginM (Either SDoc EvTerm)
produceEvidenceFor inst instArgs = do
  bindCls <- getBindClass
  instFunctor <- getBindFunctorInstance
  instApply <- getBindApplyInstance
  idTyCon <- getIdentityTyCon
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceFor bindCls instFunctor instApply idTyCon givenCts inst instArgs

-- | See 'E.isPotentiallyInstantiatedCt'.
isPotentiallyInstantiatedCt :: Ct -> [(TyVar, Either TyCon TyVar)] -> SupermonadPluginM Bool
isPotentiallyInstantiatedCt ct assoc = do
  bindCls <- getBindClass
  instFunctor <- getBindFunctorInstance
  instApply <- getBindApplyInstance
  idTyCon <- getIdentityTyCon
  givenCts <- getGivenConstraints
  runTcPlugin $ E.isPotentiallyInstantiatedCt bindCls instFunctor instApply idTyCon givenCts ct assoc

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

