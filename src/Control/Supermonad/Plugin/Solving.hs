
-- | Functions to solve wanted constraints.
module Control.Supermonad.Plugin.Solving 
  ( solveConstraints
  ) where

import Data.Maybe 
  ( catMaybes
  , isJust, isNothing
  , fromJust )
import Data.List ( partition, nubBy )
import qualified Data.Set as S

import Control.Monad ( when, forM, forM_, filterM, liftM2 )

import TcRnTypes ( Ct(..) )
import TyCon ( TyCon )
import Type 
  ( Type, TyVar, TvSubst
  , substTyVar
  , eqType )
import TcType ( isAmbiguousTyVar )
import InstEnv ( ClsInst, instanceHead )
import Unify ( tcUnifyTy )
import qualified Outputable as O


import Control.Supermonad.Plugin.Debug ( sDocToStr )
import Control.Supermonad.Plugin.Environment 
  ( SupermonadPluginM
  , getGivenConstraints, getWantedConstraints
  , getReturnClass, getBindClass
  , getSupermonadFor
  , addDerivedResult, addDerivedResults
  , addEvidenceResult
  , printMsg, printConstraints, printObj, printErr
  , throwPluginError, throwPluginErrorSDoc
  , whenNoResults )
import Control.Supermonad.Plugin.Environment.Lift
  ( produceEvidenceForCt
  , isPotentiallyInstantiatedCt
  , isBindConstraint, isReturnConstraint
  , partiallyApplyTyCons )
import Control.Supermonad.Plugin.Constraint 
  ( WantedCt, DerivedCt
  , mkDerivedTypeEqCt, mkDerivedTypeEqCtOfTypes
  , isClassConstraint
  , constraintClassType
  , constraintClassTyArgs
  , constraintTopTcVars
  , constraintTopTyCons )
import Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons, componentTopTcVars
  , componentMonoTyCon )
import Control.Supermonad.Plugin.Utils 
  ( collectTopTcVars
  , collectTyVars
  , associations
  , allM )


-- | Attempts to solve the given /wanted/ constraints.
solveConstraints :: [WantedCt] -> SupermonadPluginM ()
solveConstraints wantedCts = do
  {-
  -- Look through the constraints and see if we can find constraints that do not 
  -- contain ambiguous variables and try to produce evidence for them.
  clearedWantedCts <- fmap catMaybes $ forM wantedCts $ \ct -> do
    eEv <- produceEvidenceForCt ct
    case eEv of
      Left _err -> return $ Just ct
      Right ev -> do
        addEvidenceResult (ev, ct) -- :: (EvTerm, WantedCt) -> SupermonadPluginM ()
        return Nothing
  -}
  -- Calculate the different groups of constraints that belong 
  -- together for solving purposes.
  ctGroups <- separateContraints wantedCts
  
  -- Check each group wether it constains exactly one type constructor or not.
  markedCtGroups <- forM ctGroups $ \g -> do 
    mMonoTyCon <- componentMonoTyCon g
    return (mMonoTyCon, g)
  
  -- TODO: Filter out constraint groups that do not contain return or bind constraints.
  -- We can't solve these so we won't handle them.
  
  -- Split the groups into mono and poly groups to be solved by their 
  -- appropriate solving algorithm.
  let (monoGroups, polyGroups) = partition (isJust . fst) markedCtGroups
  forM_ (fmap (\(tc, g) -> (fromJust tc, g)) monoGroups) solveMonoConstraintGroup
  forM_ (fmap snd polyGroups) solvePolyConstraintGroup
  
  -- Finally we go through all constraints that have their ambiguous 
  -- top-level type constructors variables solved (they are actual type 
  -- constructor (variable)) and solve their indices by unification with
  -- their 'Bind' and 'Return' instances.
  solveSolvedTyConIndices



-- | Solves constraint groups that only involve exactly one supermonad type 
--   constructor. This is done by associating every ambiguous 
--   supermonad type constructor variable with that type constructor.
--   
--   If necessary the associated type constructors are partially applied 
--   to new ambiguous variables to match the kind of type variable they are 
--   associated with.
solveMonoConstraintGroup :: (TyCon, [WantedCt]) -> SupermonadPluginM ()
solveMonoConstraintGroup (_, []) = return ()
solveMonoConstraintGroup (tyCon, ctGroup) = do
  printMsg "Solve mono group..."
  --printConstraints ctGroup
  smCtGroup <- filterM (\ct -> liftM2 (||) (isReturnConstraint ct) (isBindConstraint ct)) ctGroup
  forM_ smCtGroup $ \ct -> do
    let ctAmbVars = S.filter isAmbiguousTyVar 
                  $ collectTopTcVars 
                  $ maybe [] id
                  $ constraintClassTyArgs ct
    forM_ ctAmbVars $ \tyVar -> do
      appliedTyCon <- either throwPluginErrorSDoc return =<< partiallyApplyTyCons [(tyVar, Left tyCon)]
      case nubBy tyConAssocEq appliedTyCon of
        [] -> do
          throwPluginError "How did this become an empty list?"
        [(tv, t, _)] -> do
          addDerivedResult $ mkDerivedTypeEqCt ct tv t
        _ -> do
          throwPluginError "How did this become a list with more then one element?"
  where
    tyConAssocEq :: (TyVar, Type, [TyVar]) -> (TyVar, Type, [TyVar]) -> Bool
    tyConAssocEq (tv, t, tvs) (tv', t', tvs') = tv == tv' && tvs == tvs' && eqType t t'



-- | Solves constraint groups that have more them one supermonad type 
--   constructor (variable) involved. This is done by looking at
--   all possible association between ambiguous type constructor variables
--   and available type constructors and checking their (approximate)
--   satisfiability. If there is exactly one such association that is 
--   satisfiable, constraints for it are produced, otherwise this solver step
--   aborts with an error.
--   
--   If necessary the associated type constructors are partially applied 
--   to new ambiguous variables to match the kind of type variable they are 
--   associated with.
solvePolyConstraintGroup :: [WantedCt] -> SupermonadPluginM ()
solvePolyConstraintGroup ctGroup = do
  printMsg "Solve poly group..."
  printConstraints ctGroup
  -- Find a valid associations for each of the constraint groups.
  (_, assocs) <- determineValidConstraintGroupAssocs ctGroup
  
  appliedAssocs <- forM assocs $ \assoc -> either throwPluginErrorSDoc return =<< partiallyApplyTyCons assoc
  
  -- Now check if there are constraint that left that require solving.
  case (ctGroup, appliedAssocs) of
    
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
      topTcVars <- concat <$> mapM collectBindReturnTopTcVars ctGroup
      -- The first case:
      if null topTcVars then do
        printMsg "Group does not require solving:"
        printConstraints ctGroup
      -- The second case:
      else do
        printConstraints ctGroup
        throwPluginError "There are no possible associations for the current constraint group!"
    
    -- There are constraints and exactly one association...
    (_, [appliedAssoc]) -> do
      printMsg "Derived Results:"
      forM_ appliedAssoc $ \(tv, ty, _flexVars) -> do
        let derivedRes = mkDerivedTypeEqCt (head ctGroup) tv ty
        printObj derivedRes
        addDerivedResult derivedRes
    
    -- There are constraints and more then one association...
    (_, _) -> do     
      printConstraints ctGroup
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



-- | Given a constraint group this calculates the set of type constructors
--   involved with that constraint group and then generates all potentially
--   satisfiable (see 'isPotentiallyInstantiatedCt') associations between 
--   ambiguous supermonad type variables and type constructor (variables).
determineValidConstraintGroupAssocs :: [WantedCt] -> SupermonadPluginM ([WantedCt], [[(TyVar, Either TyCon TyVar)]])
determineValidConstraintGroupAssocs [] = throwPluginError "Solving received an empty constraint group!"
determineValidConstraintGroupAssocs ctGroup = do
  -- Get the all of the given constraints.
  givenCts <- getGivenConstraints
  
  -- Only work with actual supermonad constraints (all others 
  -- don't contribute the the associations).
  smCtGroup <- filterSupermonadCts ctGroup
  
  -- Collect the ambiguous variables that require solving withing this 
  -- group of constraints.
  -- :: [TyVar]
  tyConVars <- S.toList <$> getAmbTyConVarsFrom smCtGroup
  
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
  where
    -- | Only keep supermonad constraints ('Bind' and 'Return').
    filterSupermonadCts :: [Ct] -> SupermonadPluginM [Ct]
    filterSupermonadCts cts = do
      returnCls <- getReturnClass
      bindCls <- getBindClass
      return $ filter (\ct -> isClassConstraint returnCls ct || isClassConstraint bindCls ct) cts

    -- | Filters all supermonad constraints ('Bind' and 'Return')
    --   that contain at least one of the given variables/type constructors
    --   in their arguments.
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
    
    -- | Collect all ambiguous top-level type constructor variables
    --   from the given constraints.
    getAmbTyConVarsFrom :: [Ct] -> SupermonadPluginM (S.Set TyVar)
    getAmbTyConVarsFrom cts = do
      checkedCts <- filterSupermonadCts cts
      return $ S.filter isAmbiguousTyVar $ componentTopTcVars checkedCts



-- |
solveSolvedTyConIndices :: SupermonadPluginM ()
solveSolvedTyConIndices = do
  -- Unification solve return constraints that are applied to top-level tycons.
  whenNoResults $ do
    returnCts <- getTopTyConSolvedConstraints isReturnConstraint
    forM_ returnCts $ \returnCt -> do
      eResult <- withTopTyCon returnCt $ \_topTyCon _bindCtArgs _bindInst returnInst -> do
        case deriveUnificationConstraints returnCt returnInst of
          Left err -> do
            printErr err
          Right eqCts -> do 
            printConstraints eqCts
            addDerivedResults eqCts
      case eResult of
        Left err -> printErr $ sDocToStr err
        Right () -> return ()
  
  -- Unification solve bind constraints that are applied to top-level tycons.
  whenNoResults $ do
    bindCts <- getTopTyConSolvedConstraints isBindConstraint
    forM_ bindCts $ \bindCt -> do
      eResult <- withTopTyCon bindCt $ \_topTyCon _bindCtArgs bindInst _returnInst -> do
        case deriveUnificationConstraints bindCt bindInst of
          Left err -> do
            printErr err
          Right eqCts -> do 
            printConstraints eqCts
            addDerivedResults eqCts
      case eResult of
        Left err -> printErr $ sDocToStr err
        Right () -> return ()
  
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
        instVarEqGroupsCt <- fmap concat $ forM instVarEqGroups $ \(_, eqGroup) -> do
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