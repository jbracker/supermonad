-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import Type ( Type, getTyVar, mkTyConTy )
import TyCon ( TyCon )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import Outputable ( showSDocUnsafe )

import Control.Supermonad.Plugin.Log ( pprToStr )
import Control.Supermonad.Plugin.Utils ( isAmbiguousType )
import Control.Supermonad.Plugin.Constraint
  ( mkDerivedTypeEqCt
  , constraintClassTyArgs
  , isClassConstraint )
import Control.Supermonad.Plugin.Detect
  ( areBindFunctorArguments, areBindApplyArguments )
import Control.Supermonad.Plugin.Evidence
  ( produceEvidenceFor )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM, runSupermonadPlugin
  , getIdentityTyCon
  , getReturnClass, getBindClass
  , getWantedConstraints, getGivenConstraints
  , getBindFunctorInstance, getBindApplyInstance
  , processAndRemoveWantedConstraints
  , whenNoResults
  , runTcPlugin
  , printMsg, printObj, printConstraints )

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
  return $ case res of
    Left _err -> noResult
    Right solution -> solution

-- | The actual plugin code.
supermonadSolve' :: SupermonadState -> SupermonadPluginM ()
supermonadSolve' _s = do
  getWantedConstraints >>= printConstraints
  
  processWantedReturnConstraints
  
  whenNoResults $ do
    processWantedFunctorBindConstraints
  
  -- End of plugin code.
  return ()

noResult :: TcPluginResult
noResult = TcPluginOk [] []

-- -----------------------------------------------------------------------------
-- Plugin processing steps
-- -----------------------------------------------------------------------------

processWantedReturnConstraints :: SupermonadPluginM ()
processWantedReturnConstraints = do
  -- Get information from the environment
  identityTC <- getIdentityTyCon
  returnCls <- getReturnClass
  -- Default all ambiguous type variables in 'Return' constraints
  -- to 'Identity'.
  processAndRemoveWantedConstraints (return . isClassConstraint returnCls) $ \returnCt ->
    case constraintClassTyArgs returnCt of
      Just [t] -> if isAmbiguousType t
        then do
          let ambTv = getTyVar "Type is not a TyVar" t
          return $ ([], [mkDerivedTypeEqCt returnCt ambTv (mkTyConTy identityTC)])
        else return ([], [])
      _ -> return ([], [])

processWantedFunctorBindConstraints :: SupermonadPluginM ()
processWantedFunctorBindConstraints = do
  
  processAndRemoveWantedConstraints (isBindConstraintWith areBindFunctorArguments) $ \bindCt -> do
    printMsg "Solve functor bind constraint:"
    printConstraints [bindCt]
    let Just [t, _, _] = constraintClassTyArgs bindCt
    if isAmbiguousType t then do
      printMsg "Functor bind constraint includes ambiguous variable:"
      printObj bindCt
      return ([], [])
    else do
      instFunctor <- getBindFunctorInstance
      givenCts <- getGivenConstraints
      printMsg "Given Constraints:"
      printConstraints givenCts
      eEvidence <- runTcPlugin $ produceEvidenceFor givenCts instFunctor [t]
      -- This assumes that the functor instance has the following head 
      -- "(Functor m) => Bind m Identity m" and therefore there is only one variable.
      -- produceEvidenceFor :: [GivenCt] -> ClsInst -> [Type] -> TcPluginM (Either SDoc EvTerm)
      case eEvidence of
        Right evidence -> return ([(evidence, bindCt)], [])
        Left err -> do
          printMsg "Failed to produce evidence for functor bind constraint:"
          printObj bindCt
          printMsg $ showSDocUnsafe err
          return ([], [])
  
  processAndRemoveWantedConstraints (isBindConstraintWith areBindApplyArguments) $ \bindCt -> do
    -- TODO: Pick the correct functor instance
    return ([], [])

-- -----------------------------------------------------------------------------
-- General plugin utilities
-- -----------------------------------------------------------------------------
      
isBindConstraintWith :: (TyCon -> Type -> Type -> Type -> Bool) -> Ct -> SupermonadPluginM Bool
isBindConstraintWith p ct = do
  bindCls <- getBindClass
  idTyCon <- getIdentityTyCon
  case (isClassConstraint bindCls ct, constraintClassTyArgs ct) of
    (True, Just [t1, t2, t3]) -> return $ p idTyCon t1 t2 t3
    _ -> return False
  






