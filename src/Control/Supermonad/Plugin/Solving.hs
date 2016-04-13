 
module Control.Supermonad.Plugin.Solving 
  ( solveConstraints
  ) where

import Data.Maybe ( catMaybes, fromJust )
import Data.List ( partition )
import qualified Data.Set as S

import Control.Monad ( when, forM, forM_, filterM )

import TcRnTypes ( Ct )
import TyCon ( TyCon, tyConKind )
import Type 
  ( Type, TyVar, TvSubst
  , mkTopTvSubst
  , substTys )
import Var ( tyVarKind )
import TcPluginM ( TcPluginM )
import TcType ( isAmbiguousTyVar )
import TcEvidence ( EvTerm )
import Class ( Class )
import Kind ( splitKindFunTys ) 
import InstEnv ( lookupInstEnv, instanceSig )
import Outputable ( ($$) )
import qualified Outputable as O

import Control.Supermonad.Plugin.Debug ( sDocToStr )
import Control.Supermonad.Plugin.Environment 
  ( SupermonadPluginM
  , getGivenConstraints
  , getInstEnvs
  , getReturnClass, getBindClass
  , getIdentityTyCon
  , getBindFunctorInstance, getBindApplyInstance
  , addDerivedResult, addEvidenceResult
  , runTcPlugin
  , printMsg, printConstraints, printObj
  , throwPluginError, catchPluginError
  , assertM )
import Control.Supermonad.Plugin.Environment.Lift
  ( produceEvidenceForCt
  , isPotentiallyInstantiatedCt )
import Control.Supermonad.Plugin.Constraint 
  ( WantedCt
  , mkDerivedTypeEqCt
  , isClassConstraint
  , constraintClassType )
import Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons, componentTopTcVars )
import Control.Supermonad.Plugin.Evidence 
  ( matchInstanceTyVars' )
import Control.Supermonad.Plugin.Utils 
  ( collectTyVars
  , applyTyCon
  , splitKindFunTyConTyVar
  , associations, allM )

solveConstraints :: [WantedCt] -> SupermonadPluginM ()
solveConstraints wantedCts = do
  -- Calculate the different groups of constraints that belong 
  -- together for solving purposes.
  let ctGroups = separateContraints wantedCts
  
  -- Get classes to filter constraints.
  returnCls <- getReturnClass
  bindCls <- getBindClass
  
  -- TODO: Filter out constraint groups that do not contain return or bind constraints.
  -- We can't solve these so we won't handle them.
  
  -- Find a valid associations for each of the constraint groups.
  validAssocsForGroup <- forM ctGroups $ determineValidConstraintGroupAssocs
  
  -- Partially apply type constructors in associations to fit the kind of 
  -- the type variable they are associated with.
  appliedSolvedGroups <- forM validAssocsForGroup $ \(ctGroup, assocs) -> do
    appliedAssocs <- forM assocs $ \assoc -> forM assoc $ \(tv, tc) -> do
      let (tvKindArgs, tvKindRes) = splitKindFunTys $ tyVarKind tv
      let (tcKindArgs, tcKindRes) = splitKindFunTyConTyVar tc
      assertM (return $ length tcKindArgs >= length tvKindArgs) 
              $ sDocToStr
              $ O.text "solveConstraints: Kind mismatch between type constructor and type variable: " 
              $$ O.ppr tcKindArgs $$ O.text " | " $$ O.ppr tvKindArgs
      assertM (return $ and (uncurry (==) <$> zip (reverse tvKindArgs) (reverse tcKindArgs)) && tcKindRes == tvKindRes)
              $ sDocToStr
              $ O.text "solveConstraints: Kind mismatch between type constructor and type variable: " 
              $$ O.ppr tc $$ O.text " | " $$ O.ppr tv
      -- Apply as many new type variables to the type constructor as are 
      -- necessary for its kind to match that of the type variable.
      (appliedTcTy, argVars) <- runTcPlugin $ applyTyCon (tc, take (length tcKindArgs - length tvKindArgs) tcKindArgs)
      return ((tv, appliedTcTy, argVars) :: (TyVar, Type, [TyVar]))
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
      (ctGroup, []) -> do
        printConstraints ctGroup
        throwPluginError "There are no possible associations for the current constraint group!"
      
      -- There are constraints and exactly one association...
      (ctGroup, [appliedAssoc]) -> do
        printMsg "Derived Results:"
        forM_ appliedAssoc $ \(tv, ty, _flexVars) -> do
          let derivedRes = mkDerivedTypeEqCt (head ctGroup) tv ty
          printObj derivedRes
          addDerivedResult derivedRes
      
      -- There are constraints and more then one association...
      (ctGroup, appliedAssocs) -> do     
        printConstraints ctGroup
        printMsg "Possible Assocs:"
        forM_ appliedAssocs printObj
        throwPluginError "There is more then one possible association for the current constraint group!"
    
    return ()
  
  -- Unnecessary, just there to mark end of do-block
  return ()

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
  idTyCon <- getIdentityTyCon
  return $ S.insert (Left idTyCon) $ S.union baseTvs baseTcs

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
  
  bindCls <- getBindClass
  instFunctor <- getBindFunctorInstance
  instApply <- getBindApplyInstance
  idTyCon <- getIdentityTyCon
  
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
