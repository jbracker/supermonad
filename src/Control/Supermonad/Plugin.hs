
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Data.Maybe ( catMaybes, isNothing )
import Data.List ( find )
import qualified Data.Set as S

import Control.Monad ( forM, forM_, filterM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import Type 
  ( Type, TyVar, TvSubst
  , getTyVar, mkTyConTy, substTyVar
  , eqType
  , isTyVarTy )
import TyCon ( TyCon )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import TcEvidence ( EvTerm )
import TcType ( isAmbiguousTyVar )
import InstEnv ( lookupInstEnv, instanceHead )
import Unify ( tcUnifyTy )
import Outputable ( showSDocUnsafe )

import Control.Supermonad.Plugin.Log ( pprToStr )
import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Utils 
  ( isAmbiguousType, collectTyVars, skolemVarsBindFun )
import Control.Supermonad.Plugin.Constraint
  ( WantedCt, DerivedCt
  , mkDerivedTypeEqCt
  , constraintClassTyArgs
  , constraintTopTcVars
  , sortConstraintsByLine
  , isClassConstraint )
import Control.Supermonad.Plugin.Detect
  ( areBindFunctorArguments, areBindApplyArguments )
import Control.Supermonad.Plugin.Evidence
  ( matchInstanceTyVars )
import Control.Supermonad.Plugin.Solving
  ( solveConstraints )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM, runSupermonadPlugin
  , getIdentityTyCon
  , getReturnClass, getBindClass
  , getWantedConstraints, getGivenConstraints
  , getBindFunctorInstance, getBindApplyInstance
  , getBindInstances
  , getInstEnvs
  , addEvidenceResult
  , addDerivedResults
  , processAndRemoveWantedConstraints
  , processEachWantedConstraint
  , whenNoResults
  , runTcPlugin
  , printMsg, printObj, printConstraints )
import Control.Supermonad.Plugin.Environment.Lift
  ( produceEvidenceFor
  , isBindConstraint)

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
      L.printErr err
      return noResult
    Right solution -> return solution

-- | The actual plugin code.
supermonadSolve' :: SupermonadState -> SupermonadPluginM ()
supermonadSolve' _s = do
  getWantedConstraints >>= (printConstraints . sortConstraintsByLine)
  getGivenConstraints >>= (printConstraints . sortConstraintsByLine)
  
  getWantedConstraints >>= solveConstraints 
  
  -- Old naive version...
  --whenNoResults processWantedReturnConstraints
  --whenNoResults processWantedFunctorBindConstraints
  --whenNoResults processWantedBindConstraintsWithOnlyOneMatchingInstance
  
  whenNoResults $ do
    bindCts <- filterM isBindConstraint =<< getWantedConstraints
    let solvedBindCts = filter (S.null . constraintTopTcVars) bindCts
    instEnvs <- getInstEnvs
    bindCls <- getBindClass
    forM_ solvedBindCts $ \bindCt -> do
      let Just bindCtArgs = constraintClassTyArgs bindCt
      let (lookupInstMatch, lookupInsts, _) = lookupInstEnv instEnvs bindCls bindCtArgs 
      let foundInsts = fmap fst lookupInstMatch ++ lookupInsts
      case foundInsts of
        [inst] -> do
          let (instVars, _instCls, instArgs) = instanceHead inst
          let bindCtVars = S.toList $ S.unions $ fmap collectTyVars bindCtArgs 
          printMsg "Found possible instance:"
          printObj bindCt
          printObj inst
          let mSubsts = zipWith tcUnifyTy instArgs bindCtArgs
          if any isNothing mSubsts then do
            printMsg "Missing substitution!"
          else do
            let eqGroups = collectEqualityGroup (catMaybes mSubsts) instVars
            forM_ eqGroups $ \(_, eqGroup) -> do
              let eqGroupCts = mkEqGroup bindCt eqGroup
              printObj eqGroupCts
              addDerivedResults eqGroupCts
          
        (_ : _) -> do
          printMsg "No unique instance match for solved bind constraint:"
          printObj bindCt
          printMsg "Matching instances:"
          printObj foundInsts
        [] -> do
          printMsg "No matching instance for bind constraint:"
          printObj bindCt
          
  
  -- End of plugin code.
  return ()
  where
    
    mkEqStar :: Ct -> TyVar -> [Type] -> [DerivedCt]
    mkEqStar baseCt tv tys = fmap (mkDerivedTypeEqCt baseCt tv) tys
      
    mkEqGroup ::  Ct -> [Type] -> [DerivedCt]
    mkEqGroup baseCt tys = case findAndRemove isTyVarTy tys of
      Just (t, tys') -> mkEqStar baseCt (getTyVar "Plugin.hs: Should never happen!" t) tys'
      Nothing -> []
      
    findAndRemove :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findAndRemove p [] = Nothing
    findAndRemove p (a:as) = 
      if p a 
        then Just (a, as) 
        else do
          (foundA, as') <- findAndRemove p as
          return (foundA, a : as')
      
    collectEqualityGroup :: [TvSubst] -> [TyVar] -> [(TyVar, [Type])]
    collectEqualityGroup substs tvs = [ (tv, [ substTyVar subst tv | subst <- substs]) | tv <- tvs]

noResult :: TcPluginResult
noResult = TcPluginOk [] []

-- -----------------------------------------------------------------------------
-- Plugin processing steps
-- -----------------------------------------------------------------------------
-- Functions for the naive version of the solving algorithm.

{-
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
    let Just [_, _, t] = constraintClassTyArgs bindCt
    if isAmbiguousType t then do
      return ([], [])
    else do
      instFunctor <- getBindFunctorInstance
      -- This assumes that the functor instance has the following head 
      -- "(Functor m) => Bind m Identity m" and therefore there is only one variable.
      eEvidence <- produceEvidenceFor instFunctor [t]
      case eEvidence of
        Right evidence -> return ([(evidence, bindCt)], [])
        Left err -> do
          printMsg "Failed to produce evidence for functor bind constraint:"
          printObj bindCt
          printMsg $ showSDocUnsafe err
          return ([], [])
  
  processAndRemoveWantedConstraints (isBindConstraintWith areBindApplyArguments) $ \bindCt -> do
    let Just [_, _, t] = constraintClassTyArgs bindCt
    if isAmbiguousType t then do
      return ([], [])
    else do
      instApply <- getBindApplyInstance
      -- This assumes that the functor instance has the following head 
      -- "(Functor m) => Bind Identity m m" and therefore there is only one variable.
      eEvidence <- produceEvidenceFor instApply [t]
      case eEvidence of
        Right evidence -> return ([(evidence, bindCt)], [])
        Left err -> do
          printMsg "Failed to produce evidence for functor apply bind constraint:"
          printObj bindCt
          printMsg $ showSDocUnsafe err
          return ([], [])

processWantedBindConstraintsWithOnlyOneMatchingInstance :: SupermonadPluginM ()
processWantedBindConstraintsWithOnlyOneMatchingInstance =
  processEachWantedConstraint $ \wantedCt -> do
    bindCls <- getBindClass
    if isClassConstraint bindCls wantedCt then do
      eEv <- selectOnlyMatchingBindInstance wantedCt
      case eEv of
        Just (ev, eqs) -> do
          printMsg "ONLY MATCHING INSTANCE SELECTED:"
          printObj $ snd ev
          addEvidenceResult ev
          addDerivedResults eqs
          return True -- Discard
        Nothing -> return False -- Keep
    else return False -- Keep


selectOnlyMatchingBindInstance :: WantedCt -> SupermonadPluginM (Maybe ((EvTerm, WantedCt), [Ct]))
selectOnlyMatchingBindInstance wantedCt = do
  case constraintClassTyArgs wantedCt of
    Just tyArgs | all (not . isAmbiguousTyVar) (concatMap (S.toList . collectTyVars) tyArgs) -> do
      bindInsts <- getBindInstances
      mFoundInstEvs <- forM bindInsts $ \bindInst -> 
        case matchInstanceTyVars bindInst tyArgs of
          Just (instVariableArgs, ambEqs) -> do
            eResult <- produceEvidenceFor bindInst instVariableArgs -- SupermonadPluginM (Either SDoc EvTerm)
            -- mkDerivedTypeEqCt :: Ct -> TyVar -> Type -> Ct
            return $ case eResult of
              Left _err -> Nothing
              Right ev -> Just (bindInst, (ev, wantedCt), fmap (uncurry $ mkDerivedTypeEqCt wantedCt) ambEqs)
          Nothing -> return Nothing
      let foundInstEvs = catMaybes mFoundInstEvs
      -- Only keep those matches that actually found a type for every argument.
      case fmap (\(_, ev, eqs) -> (ev, eqs)) foundInstEvs of
        -- Only one matching instance, try to use it...
        [ev] -> return $ Just ev
        -- More then one or no matching instance...
        _ -> return Nothing
    _ -> return Nothing

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
  
-}






