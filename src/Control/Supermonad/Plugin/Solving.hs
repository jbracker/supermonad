
-- | Functions to solve wanted constraints.
module Control.Supermonad.Plugin.Solving 
  ( solveConstraints
  ) where

import Data.Maybe ( catMaybes )
import qualified Data.Set as S

import Control.Monad ( when, forM, forM_, filterM, liftM2 )

import TcRnTypes ( Ct )
import TyCon ( TyCon )
import Type ( TyVar )
import TcType ( isAmbiguousTyVar )

-- import Control.Supermonad.Plugin.Debug ( sDocToStr )
import Control.Supermonad.Plugin.Environment 
  ( SupermonadPluginM
  , getGivenConstraints
  , getReturnClass, getBindClass
  , getIdentityTyCon
  , addDerivedResult, addEvidenceResult
  , printMsg, printConstraints, printObj
  , throwPluginError, throwPluginErrorSDoc )
import Control.Supermonad.Plugin.Environment.Lift
  ( produceEvidenceForCt
  , isPotentiallyInstantiatedCt
  , isBindConstraint, isReturnConstraint
  , partiallyApplyTyCons )
import Control.Supermonad.Plugin.Constraint 
  ( WantedCt
  , mkDerivedTypeEqCt
  , isClassConstraint
  , constraintClassType )
import Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons, componentTopTcVars )
import Control.Supermonad.Plugin.Utils 
  ( collectTopTcVars
  , associations, allM )

-- | Attempts to solve the given group /wanted/ constraints. See 'separateContraints'.
solveConstraints :: [WantedCt] -> SupermonadPluginM ()
solveConstraints wantedCts = do
  -- Calculate the different groups of constraints that belong 
  -- together for solving purposes.
  ctGroups <- separateContraints wantedCts
  
  -- TODO: Filter out constraint groups that do not contain return or bind constraints.
  -- We can't solve these so we won't handle them.
  
  -- Find a valid associations for each of the constraint groups.
  validAssocsForGroup <- forM ctGroups $ determineValidConstraintGroupAssocs
  
  -- Partially apply type constructors in associations to fit the kind of 
  -- the type variable they are associated with.
  appliedSolvedGroups <- forM validAssocsForGroup $ \(ctGroup, assocs) -> do
    appliedAssocs <- forM assocs $ \assoc -> either throwPluginErrorSDoc return =<< partiallyApplyTyCons assoc
    return (ctGroup, appliedAssocs)
    
  -- For each group, try to produde evidence for each involved constraint.
  forM_ appliedSolvedGroups $ \(ctGroup, appliedAssocs) -> do
    -- Look through the group and see if we can find constraints that do not 
    -- contain ambiguous variables and try to produce evidence for them.
    clearedCtGroup <- fmap catMaybes $ forM ctGroup $ \ct -> do
      eEv <- produceEvidenceForCt ct
      case eEv of
        Left _err -> return $ Just ct
        Right ev -> do
          addEvidenceResult (ev, ct) -- :: (EvTerm, WantedCt) -> SupermonadPluginM ()
          return Nothing
    
    -- Now check if there are constraint that left that require solving.
    case (clearedCtGroup, appliedAssocs) of
      
      -- No constraints left, we are done.
      ([], _) -> return ()
      
      -- There are constraints, but there are no associations...
      -- There are two possible reasons for this to happen:
      -- 1. The group does not have type constructor variables. 
      --    Which means there is nothing to solve and therefore 
      --    we are done with solving. 
      -- 2. There are no associations, because we can't find a solution
      --    to the variables in the group. If this is the case the group 
      --    is unsolvable.
      (_, []) -> do
        topTcVars <- concat <$> mapM collectBindReturnTopTcVars clearedCtGroup
        -- The first case:
        if null topTcVars then do
          printMsg "Group does not require solving:"
          printConstraints clearedCtGroup
        -- The second case:
        else do
          printConstraints clearedCtGroup
          throwPluginError "There are no possible associations for the current constraint group!"
      
      -- There are constraints and exactly one association...
      (_, [appliedAssoc]) -> do
        printMsg "Derived Results:"
        forM_ appliedAssoc $ \(tv, ty, _flexVars) -> do
          let derivedRes = mkDerivedTypeEqCt (head clearedCtGroup) tv ty
          printObj derivedRes
          addDerivedResult derivedRes
      
      -- There are constraints and more then one association...
      (_, _) -> do     
        printConstraints clearedCtGroup
        printMsg "Possible Assocs:"
        forM_ appliedAssocs printObj
        throwPluginError "There is more then one possible association for the current constraint group!"
  
  where
    collectBindReturnTopTcVars :: Ct -> SupermonadPluginM [TyVar]
    collectBindReturnTopTcVars ct = do
      isBindOrReturn <- liftM2 (||) (isBindConstraint ct) (isReturnConstraint ct)
      case (isBindOrReturn, constraintClassType ct) of
        (True, Just (_cls, tyArgs)) -> return $ S.toList $ collectTopTcVars tyArgs
        _ -> return []


-- | Only keep supermonad constraints ('Bind' and 'Return').
filterSupermonadCts :: [Ct] -> SupermonadPluginM [Ct]
filterSupermonadCts cts = do
  returnCls <- getReturnClass
  bindCls <- getBindClass
  return $ filter (\ct -> isClassConstraint returnCls ct || isClassConstraint bindCls ct) cts

filterSupermonadCtsWith :: [Ct] -> S.Set (Either TyCon TyVar) -> SupermonadPluginM [Ct]
filterSupermonadCtsWith allCts baseTyCons = do
  cts <- filterSupermonadCts allCts
  filterM predicate cts
  where 
    predicate :: Ct -> SupermonadPluginM Bool
    predicate ct = do
      ctBase <- getTyConBaseFrom [ct]
      return $ not $ S.null $ S.intersection ctBase baseTyCons

-- | Calculate the type constructor base for the given constraint.
getTyConBaseFrom :: [Ct] -> SupermonadPluginM (S.Set (Either TyCon TyVar))
getTyConBaseFrom cts = do
  checkedCts <- filterSupermonadCts cts
  let baseTvs :: S.Set (Either TyCon TyVar)
      baseTvs = S.map Right $ S.filter (not . isAmbiguousTyVar) $ componentTopTcVars checkedCts
  let baseTcs :: S.Set (Either TyCon TyVar)
      baseTcs = S.map Left $ componentTopTyCons checkedCts
  return $ S.union baseTvs baseTcs

getTyConVarsFrom :: [Ct] -> SupermonadPluginM (S.Set TyVar)
getTyConVarsFrom cts = do
  checkedCts <- filterSupermonadCts cts
  return $ S.filter isAmbiguousTyVar $ componentTopTcVars checkedCts

determineValidConstraintGroupAssocs :: [WantedCt] -> SupermonadPluginM ([WantedCt], [[(TyVar, Either TyCon TyVar)]])
determineValidConstraintGroupAssocs [] = throwPluginError "Solving received an empty constraint group!"
determineValidConstraintGroupAssocs ctGroup = do
  -- Get the all of the given constraints.
  givenCts <- getGivenConstraints
  
  smCtGroup <- filterSupermonadCts ctGroup
  
  -- Collect the ambiguous variables that require solving withing this 
  -- group of constraints.
  -- :: [TyVar]
  tyConVars <- S.toList <$> getTyConVarsFrom smCtGroup
  
  -- Calculate the type constructor base used for solving. That means
  -- calculate the set of supermonad type constructors that are involved 
  -- with this group of constraints.
  -- :: S.Set (Either TyCon TyVar)
  wantedTyConBase <- getTyConBaseFrom smCtGroup
  -- :: S.Set (Either TyCon TyVar)
  givenTyConBase <- getTyConBaseFrom =<< filterSupermonadCtsWith givenCts wantedTyConBase
  
  let tyConBase :: [Either TyCon TyVar]
      tyConBase = S.toList $ S.union wantedTyConBase givenTyConBase
  
  -- All possible associations of ambiguous variables with their possible 
  -- type constructors from the base.
  -- Also remove the empty association if it is there.
  let assocs :: [[(TyVar, Either TyCon TyVar)]]
      assocs = filter (not . null) $ associations $ fmap (\tv -> (tv, tyConBase)) tyConVars
  
  -- Debugging output
  printMsg $ "Solving Group..."
  printMsg $ "Size = " ++ show (length ctGroup) ++ "; "
           ++ "Vars = " ++ show (length tyConVars) ++ "; "
           ++ "Base Size = " ++ show (length tyConBase) ++ "; "
           ++ "Associations = " ++ show (length assocs) ++ ";"
  printMsg "Base:"
  printObj tyConBase
  when (length assocs <= 5 && not (null assocs)) $ do
    printMsg "Assocs:"
    forM_ assocs printObj
  
  -- For each association check if all constraints are potentially instantiable 
  -- with that association.
  checkedAssocs <- forM assocs $ \assoc -> do
    -- isPotentiallyInstantiatedCt :: [GivenCt] -> Ct -> [(TyVar, TyCon)] -> TcPluginM Bool
    validAssoc <- allM (\ct -> isPotentiallyInstantiatedCt ct assoc) ctGroup
    return (assoc, validAssoc)
  
  -- Only keep those associations that could be satisfiable. 
  let validAssocs = fmap fst $ filter snd checkedAssocs
  
  -- Output for debugging:
  when (length validAssocs >= 1) $ do
    printMsg "Current Constraint Group:"
    printConstraints ctGroup
    printMsg "Satisfiable associations:"
    forM_ validAssocs printObj
  
  return (ctGroup, validAssocs)
