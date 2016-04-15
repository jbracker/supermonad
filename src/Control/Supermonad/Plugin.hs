
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Data.Maybe ( catMaybes, isNothing )
import qualified Data.Set as S

import Control.Monad ( forM_, filterM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import Type 
  ( Type, TyVar, TvSubst
  , getTyVar, substTyVar
  , isTyVarTy )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import InstEnv ( lookupInstEnv, instanceHead )
import Unify ( tcUnifyTy )

import Control.Supermonad.Plugin.Log ( sDocToStr )
import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Utils 
  ( collectTyVars )
import Control.Supermonad.Plugin.Constraint
  ( DerivedCt
  , mkDerivedTypeEqCt
  , constraintClassTyArgs
  , constraintTopTcVars
  , sortConstraintsByLine )
import Control.Supermonad.Plugin.Solving
  ( solveConstraints )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM, runSupermonadPlugin
  , getIdentityTyCon
  , getReturnClass, getBindClass
  , getWantedConstraints, getGivenConstraints
  , getBindInstances
  , getInstEnvs
  , addEvidenceResult
  , addDerivedResults
  , whenNoResults
  , runTcPlugin
  , printMsg, printObj, printConstraints )
import Control.Supermonad.Plugin.Environment.Lift
  ( isBindConstraint)

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
  --getWantedConstraints >>= (printConstraints . sortConstraintsByLine)
  --getGivenConstraints >>= (printConstraints . sortConstraintsByLine)
  
  
  
  getWantedConstraints >>= solveConstraints 
  
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
    findAndRemove _ [] = Nothing
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







