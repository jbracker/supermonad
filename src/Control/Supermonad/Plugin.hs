
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
import Type 
  ( Type, TyVar, TvSubst
  , getTyVar, substTyVar
  , isTyVarTy
  , eqType )
import TyCon ( TyCon )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import TcType ( isAmbiguousTyVar )
import InstEnv ( ClsInst, lookupInstEnv, instanceHead )
import Unify ( tcUnifyTy )
import qualified Outputable as O

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
  
  
  
  getWantedConstraints >>= solveConstraints 
  
  -- Unification solve return constraints that are applied to top-level tycons.
  whenNoResults $ do
    returnCts <- getTopTyConSolvedConstraints isReturnConstraint
    forM_ returnCts $ \returnCt -> do
      eResult <- withTopTyCon returnCt $ \topTyCon bindCtArgs bindInst returnInst -> do
        case deriveUnificationConstraints returnCt returnInst of
          Left err -> do
            printMsg err
          Right eqCts -> do 
            printConstraints eqCts
            addDerivedResults eqCts
      case eResult of
        Left err -> printMsg $ sDocToStr err
        Right () -> return ()
  
  -- Unification solve bind constraints that are applied to top-level tycons.
  whenNoResults $ do
    bindCts <- getTopTyConSolvedConstraints isBindConstraint
    forM_ bindCts $ \bindCt -> do
      eResult <- withTopTyCon bindCt $ \topTyCon bindCtArgs bindInst returnInst -> do
        case deriveUnificationConstraints bindCt bindInst of
          Left err -> do
            printMsg err
          Right eqCts -> do 
            printConstraints eqCts
            addDerivedResults eqCts
      case eResult of
        Left err -> printMsg $ sDocToStr err
        Right () -> return ()
  
  -- End of plugin code.
  --printMsg "END PLUGIN"
  --printConstraints =<< getDerivedResults
  --printMsg "END OUTPUT"
  return ()
  where
    getTopTyConSolvedConstraints :: (WantedCt -> SupermonadPluginM Bool) -> SupermonadPluginM [WantedCt]
    getTopTyConSolvedConstraints p = do
      -- Get all wanted bind constraints.
      bindCts <- filterM p =<< getWantedConstraints
      -- Get all bind constraints that already have been assigned their top-level
      -- type constructors and process them to solve the type constructor arguments...
      return $ filter (S.null . constraintTopTcVars) bindCts
    
    withTopTyCon :: WantedCt -> ( TyCon -> [Type] -> ClsInst -> ClsInst -> SupermonadPluginM a ) -> SupermonadPluginM (Either O.SDoc a)
    withTopTyCon ct process = do
      let mCtArgs = constraintClassTyArgs ct
      -- Get the top-level type constructor for this bind constraint.
      let mTopTyCon = S.toList $ constraintTopTyCons ct
      -- Begin error handling.
      case (mCtArgs, mTopTyCon) of
        (Just ctArgs, [topTyCon]) -> do
          -- Get the bind instance for the current type constructor.
          mSupermonadInst <- getSupermonadFor topTyCon
          case mSupermonadInst of
            Just (bindInst, returnInst) -> Right <$> process topTyCon ctArgs bindInst returnInst
            -- We could not find a bind and return instance associated with the 
            -- given type constructor.
            Nothing -> do
              return $ Left
                     $ O.text "Constraints top type constructor does not form a supermonad:"
                       O.$$ O.ppr topTyCon
        (Nothing, _) -> do
          return $ Left 
                 $ O.text "Constraint is not a class constraint:"
                 O.$$ O.ppr ct
        (_, _) -> do
          return $ Left
                 $ O.text "Constraint misses a unqiue top-level type constructor:"
                 O.$$ O.ppr ct
    
    deriveUnificationConstraints :: WantedCt -> ClsInst -> Either String [DerivedCt]
    deriveUnificationConstraints ct inst = do
      let (instVars, _instCls, instArgs) = instanceHead inst
      let Just ctArgs = constraintClassTyArgs ct
      let ctVars = S.toList $ S.unions $ fmap collectTyVars ctArgs
      let mSubsts = zipWith tcUnifyTy instArgs ctArgs
      if any isNothing mSubsts then do
        Left "Missing substitution!"
      else do
        let substs = catMaybes mSubsts
        let instVarEqGroups = collectEqualityGroup substs instVars
        instVarEqGroupsCt <- map concat $ forM instVarEqGroups $ \(_, eqGroup) -> do
          return $ mkEqGroup ct eqGroup
        -- There may still the type variables from the constraint that were unified
        -- with constants. Also create type equalities for these.
        let ctVarEqGroups = collectEqualityGroup substs $ filter isAmbiguousTyVar ctVars
        let ctVarEqCts = mkEqStarGroup ct ctVarEqGroups
        return $ instVarEqGroupsCt ++ ctVarEqCts
    
    mkEqGroup ::  Ct -> [Type] -> [DerivedCt]
    mkEqGroup _ [] = []
    mkEqGroup baseCt (ty : tys) = fmap (mkDerivedTypeEqCtOfTypes baseCt ty) tys
    
    mkEqStarGroup :: Ct -> [(TyVar, [Type])] -> [DerivedCt]
    mkEqStarGroup baseCt eqGroups = concatMap (\(tv, tys) -> fmap (mkDerivedTypeEqCt baseCt tv) tys) eqGroups
    
    collectEqualityGroup :: [TvSubst] -> [TyVar] -> [(TyVar, [Type])]
    collectEqualityGroup substs tvs = [ (tv, nubBy eqType $ filter (all (tv /=) . collectTyVars) 
                                                          $ [ substTyVar subst tv | subst <- substs]
                                        ) | tv <- tvs]

noResult :: TcPluginResult
noResult = TcPluginOk [] []







