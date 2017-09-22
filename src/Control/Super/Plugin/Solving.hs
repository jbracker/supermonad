
-- | Functions to solve wanted constraints.
module Control.Super.Plugin.Solving 
  ( solveConstraints
  ) where

import Data.Maybe 
  ( catMaybes
  , isJust, isNothing
  , fromJust )
import Data.List ( partition, nubBy )
import qualified Data.Set as Set

import Control.Monad ( forM, forM_, filterM )

import TcRnTypes ( Ct(..) )
import TyCon ( TyCon )
import Class ( Class, classTyCon )
import Type 
  ( Type, TyVar
  , substTyVar, substTys
  , eqType )
import TcType ( isAmbiguousTyVar )
import InstEnv ( ClsInst, instanceHead )
import Unify ( tcUnifyTy )
import qualified Outputable as O


import qualified Control.Super.Plugin.Collection.Set as S
import Control.Super.Plugin.Debug ( sDocToStr )
import Control.Super.Plugin.InstanceDict ( InstanceDict )
import Control.Super.Plugin.Wrapper 
  ( TypeVarSubst, mkTypeVarSubst )
import Control.Super.Plugin.Environment 
  ( SupermonadPluginM
  , getGivenConstraints, getWantedConstraints
  , getInstanceFor
  , addTyVarEquality, addTyVarEqualities
  , addTypeEqualities
  , getTyVarEqualities
  , printMsg, printObj, printErr --, printConstraints
  , addWarning, displayWarnings
  , throwPluginError, throwPluginErrorSDoc )
import Control.Super.Plugin.Environment.Lift
  ( isPotentiallyInstantiatedCt
  , partiallyApplyTyCons )
import Control.Super.Plugin.Constraint 
  ( WantedCt
  , isClassConstraint
  , isAnyClassConstraint
  , constraintClassType
  , constraintClassTyArgs )
import Control.Super.Plugin.Separation 
  ( ConstraintGroup
  , separateContraints
  , componentTopTyCons, componentTopTcVars
  , componentMonoTyCon )
import Control.Super.Plugin.Utils 
  ( collectTopTcVars
  , collectTopTyCons
  , collectTyVars
  , associations
  , allM )
import Control.Super.Plugin.Log 
  ( formatConstraint )

  
-- | Attempts to solve the given /wanted/ constraints in a constraint group.
--   The given list of classes indicates which are required to be solved.
solveConstraints :: [Class] -> ConstraintGroup -> SupermonadPluginM InstanceDict ()
solveConstraints relevantClss wantedCts = do
  -- Calculate the different groups of constraints that belong 
  -- together for solving purposes.
  let ctGroups = separateContraints wantedCts
  
  -- Check each group wether it constains exactly one type constructor or not.
  let markedCtGroups = fmap (\g -> (componentMonoTyCon relevantClss g, g)) ctGroups
  
  -- TODO: Filter out constraint groups that do not contain return or bind constraints.
  -- We can't solve these so we won't handle them.
  
  -- Split the groups into mono and poly groups to be solved by their 
  -- appropriate solving algorithm.
  let (monoGroups, polyGroups) = partition (isJust . fst) markedCtGroups
  forM_ (fmap (\(tc, g) -> (fromJust tc, g)) monoGroups) $ solveMonoConstraintGroup relevantClss
  forM_ (fmap snd polyGroups) $ solvePolyConstraintGroup relevantClss
  
  -- We go through all constraints that have their ambiguous 
  -- top-level type constructors variables solved (they are an actual type 
  -- constructor (variable)) and solve their indices by unification with
  -- their associated instances.
  solveSolvedTyConIndices relevantClss
  
  -- Finally, display warnings if no progress could be made.
  displayWarnings



-- | Solves constraint groups that only involve exactly one supermonad type 
--   constructor. This is done by associating every ambiguous 
--   supermonad type constructor variable with that type constructor.
--   
--   If necessary the associated type constructors are partially applied 
--   to new ambiguous variables to match the kind of type variable they are 
--   associated with.
--   
--   Only solves the constraint of the given classes.
solveMonoConstraintGroup :: [Class] -> (TyCon, ConstraintGroup) -> SupermonadPluginM s ()
solveMonoConstraintGroup _relevantClss (_, []) = return ()
solveMonoConstraintGroup relevantClss (tyCon, ctGroup) = do
  --printMsg "Solve mono group..."
  let smCtGroup = filter (isAnyClassConstraint relevantClss) ctGroup
  forM_ smCtGroup $ \ct -> do
    let ctAmbVars = Set.filter isAmbiguousTyVar 
                  $ collectTopTcVars 
                  $ maybe [] id
                  $ constraintClassTyArgs ct
    forM_ ctAmbVars $ \tyVar -> do
      appliedTyCon <- either throwPluginErrorSDoc return =<< partiallyApplyTyCons [(tyVar, Left tyCon)]
      case nubBy tyConAssocEq appliedTyCon of
        [] -> do
          throwPluginError "How did this become an empty list?"
        [(tv, ty, _)] -> do
          addTyVarEquality ct tv ty
        _ -> do
          throwPluginError "How did this become a list with more then one element?"
  where
    -- Check if the two associations between a type variable and a type are the same association.
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
solvePolyConstraintGroup :: [Class] -> ConstraintGroup -> SupermonadPluginM s ()
solvePolyConstraintGroup relevantClss ctGroup = do
  --printMsg "Solve poly group..."
  -- Find a valid associations for each of the constraint groups.
  (_, assocs) <- determineValidConstraintGroupAssocs relevantClss ctGroup
  
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
    --    is either unsolvable or we need to let the type checker make
    --    progress using the other solved groups. Therefore, we only queue
    --    a warning in this case in the hope that the next iteration 
    --    of the type checker resolves the issue.
    (_, []) -> do
      let topTcVars = concat $ fmap collectRelevantTopTcVars ctGroup
      -- The first case:
      if null topTcVars then do
        --printMsg "Group does not require solving:"
        --printConstraints ctGroup
        return ()
      -- The second case:
      else do
        addWarning 
          "There are no possible associations for the current constraint group!" 
          ( O.hang (O.text "There are two possible reasons for this warning:") 2 
          $ O.vcat $ 
            [ O.text "1. Either the group can not be solved or"
            , O.text "2. further iterations between the plugin and type checker "
            , O.text "   have to resolve for sufficient information to arise."
            ] ++ fmap (O.text . formatConstraint) ctGroup)
    -- There are constraints and exactly one association...
    (_, [appliedAssoc]) -> do
      forM_ appliedAssoc $ \(tv, ty, _flexVars) -> do
        addTyVarEquality (head ctGroup) tv ty
    
    -- There are constraints and more then one association...
    (_, _) -> do
      printMsg "Possible associations:"
      forM_ appliedAssocs printObj
      throwPluginError "There is more then one possible association for the current constraint group!"
  where
    collectRelevantTopTcVars :: Ct -> [TyVar]
    collectRelevantTopTcVars ct = do
      let isRelevantCt = isAnyClassConstraint relevantClss ct
      case (isRelevantCt, constraintClassType ct) of
        (True, Just (_cls, tyArgs)) -> Set.toList $ collectTopTcVars tyArgs
        _ -> []


-- = Set (Either TyCon TyVar)
type TcTvSet = (S.Set TyCon, Set.Set TyVar)

tctvIntersection :: TcTvSet -> TcTvSet -> TcTvSet
tctvIntersection (tca, tva) (tcb, tvb) = (S.intersection tca tcb, Set.intersection tva tvb)

tctvNull :: TcTvSet -> Bool
tctvNull (tcs, tvs) = S.null tcs && Set.null tvs

tctvUnion :: TcTvSet -> TcTvSet -> TcTvSet
tctvUnion (tca, tva) (tcb, tvb) = (S.union tca tcb, Set.union tva tvb)

tctvToList :: TcTvSet -> [Either TyCon TyVar]
tctvToList (tcs, tvs) = (fmap Left $ S.toList tcs) ++ (fmap Right $ Set.toList tvs)

-- | Given a constraint group this calculates the set of type constructors
--   involved with that constraint group and then generates all potentially
--   satisfiable (see 'isPotentiallyInstantiatedCt') associations between 
--   ambiguous supermonad type variables and type constructor (variables).
determineValidConstraintGroupAssocs :: [Class] -> ConstraintGroup -> SupermonadPluginM s ([WantedCt], [[(TyVar, Either TyCon TyVar)]])
determineValidConstraintGroupAssocs _relevantClss [] = throwPluginError "Solving received an empty constraint group!"
determineValidConstraintGroupAssocs relevantClss ctGroup = do
  -- Get the all of the given constraints.
  givenCts <- getGivenConstraints
  
  -- Only work with relevant constraints (all others 
  -- don't contribute the the associations).
  let smCtGroup = filter (isAnyClassConstraint relevantClss) ctGroup
  
  -- Collect the ambiguous variables that require solving withing this 
  -- group of constraints.
  -- :: [TyVar]
  tyConVars <- Set.toList <$> getAmbTyConVarsFrom smCtGroup
  
  -- Calculate the type constructor base used for solving. That means
  -- calculate the set of supermonad type constructors that are involved 
  -- with this group of constraints.
  -- :: S.Set (Either TyCon TyVar)
  let wantedTyConBase = getTyConBaseFrom smCtGroup
  -- :: S.Set (Either TyCon TyVar)
  let givenTyConBase = getTyConBaseFrom $ filterRelevantCtsWith givenCts wantedTyConBase
  
  let tyConBase :: [Either TyCon TyVar]
      tyConBase = tctvToList $ tctvUnion wantedTyConBase givenTyConBase
  
  -- All possible associations of ambiguous variables with their possible 
  -- type constructors from the base.
  -- Also remove the empty association if it is there.
  let assocs :: [[(TyVar, Either TyCon TyVar)]]
      assocs = filter (not . null) $ associations $ fmap (\tv -> (tv, tyConBase)) tyConVars
  
  -- Debugging output
  {-
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
  -}
  -- For each association check if all constraints are potentially instantiable 
  -- with that association.
  checkedAssocs <- forM assocs $ \assoc -> do
    -- isPotentiallyInstantiatedCt :: [GivenCt] -> Ct -> [(TyVar, TyCon)] -> TcPluginM Bool
    validAssoc <- allM (\ct -> isPotentiallyInstantiatedCt ct assoc) ctGroup
    return (assoc, validAssoc)
  
  -- Only keep those associations that could be satisfiable. 
  let validAssocs = fmap fst $ filter snd checkedAssocs
  {- Output for debugging:
  when (length validAssocs >= 1) $ do
    printMsg "Current Constraint Group:"
    printConstraints ctGroup
    printMsg "Satisfiable associations:"
    forM_ validAssocs printObj
  -}
  return (ctGroup, validAssocs)
  where
    -- | Filters all relevant constraints that contain at least 
    --   one of the given variables/type constructors in their arguments.
    filterRelevantCtsWith :: [Ct] -> TcTvSet -> [Ct]
    filterRelevantCtsWith allCts baseTyCons =
      let cts = filter (isAnyClassConstraint relevantClss) allCts
      in filter predicate cts
      where 
        predicate :: Ct -> Bool
        predicate ct =
          let ctBase = getTyConBaseFrom [ct]
          in not $ tctvNull $ tctvIntersection ctBase baseTyCons
    
    -- | Calculate the type constructor base for the given constraint.
    getTyConBaseFrom :: [Ct] -> TcTvSet
    getTyConBaseFrom cts =
      let checkedCts = filter (isAnyClassConstraint relevantClss)  cts
          baseTvs :: Set.Set TyVar
          baseTvs = Set.filter (not . isAmbiguousTyVar) $ componentTopTcVars checkedCts
          baseTcs :: S.Set TyCon
          baseTcs = componentTopTyCons checkedCts
      in (baseTcs, baseTvs)
    
    -- | Collect all ambiguous top-level type constructor variables
    --   from the given constraints.
    getAmbTyConVarsFrom :: [Ct] -> SupermonadPluginM s (Set.Set TyVar)
    getAmbTyConVarsFrom cts = do
      let checkedCts = filter (isAnyClassConstraint relevantClss) cts
      return $ Set.filter isAmbiguousTyVar $ componentTopTcVars checkedCts



-- | Solves the indices of constraints that have been associated a type 
--   constructor.
solveSolvedTyConIndices :: [Class] -> SupermonadPluginM InstanceDict ()
solveSolvedTyConIndices relevantClss = do
  -- Get the type equalities that were determined up until now 
  -- and create a substitution from them.
  tyVarEqs <- getTyVarEqualities
  let tvSubst = mkTypeVarSubst $ fmap (\(_ct, tv, ty) -> (tv, ty)) tyVarEqs
  
  -- Get all of the wanted constraints and prepare them for processing
  -- by spliting out there arguments and substiuting the already determined 
  -- type variable equalities.
  wantedCts <- getWantedConstraints
  let prepWantedCts = catMaybes $ fmap (prepCt tvSubst) wantedCts
  
  -- Unification solve constraints that are applied to top-level tycons.
  printMsg "Unification solve constraints..."
  forM_ relevantClss $ \cls -> unificationSolve prepWantedCts (return . isClassConstraint cls) (\tc -> getInstanceFor tc cls)
  --unificationSolve prepWantedCts isReturnConstraint (\tc -> getSupermonadReturnFor tc)
  
  -- Unification solve bind constraints that are applied to top-level tycons.
  --printMsg "Unification solve bind constraints..."
  --unificationSolve prepWantedCts isBindConstraint (\tc -> getSupermonadBindFor tc)
  
  where
    unificationSolve :: [(Ct, TyCon, [Type])] 
                     -- ^ Prepared and substituted constraints.
                     -> (Ct -> SupermonadPluginM s Bool) 
                     -- ^ Filter predicate to filter the relevant constraints.
                     -> (TyCon -> SupermonadPluginM s (Maybe ClsInst)) 
                     -- ^ Lookup of the instances that is used to solve the relevant constraints.
                     -> SupermonadPluginM s ()
    unificationSolve prepWantedCts isRequiredConstraint getTyConInst = do
      cts <- filterTopTyConSolvedConstraints prepWantedCts isRequiredConstraint
      forM_ cts $ \ct -> do
        eResult <- withTopTyCon ct getTyConInst $ \_topTyCon _ctArgs inst -> do
          case deriveUnificationConstraints ct inst of
            Left err -> do
              printErr $ sDocToStr err
            Right (tvTyEqs, tyEqs) -> do
              addTyVarEqualities tvTyEqs
              addTypeEqualities tyEqs
        case eResult of
          Left err -> printErr $ sDocToStr err
          Right () -> return ()
    
    
    filterTopTyConSolvedConstraints :: [(WantedCt, TyCon, [Type])] 
                                    -> (WantedCt -> SupermonadPluginM s Bool) 
                                    -> SupermonadPluginM s [(WantedCt, TyCon, [Type])]
    filterTopTyConSolvedConstraints cts p = do
      -- Get all wanted constraints that meet the predicate.
      predFilteredCts <- filterM (\(ct, _tc, _args) -> p ct) cts
      -- Also remove constraints that don't constain any ambiguous type variables.
      -- There is nothing to solve in them.
      let filterNoVarCts = filter (\(_ct, _tc, args) -> not $ Set.null 
                                                            $ Set.filter isAmbiguousTyVar 
                                                            $ Set.unions 
                                                            $ fmap collectTyVars args) 
                                  predFilteredCts
      -- Get all constraints that already have been assigned their top-level
      -- type constructors and process them to solve the type constructor arguments...
      return $ filter (Set.null . collectTopTcVars . (\(_ct, _tc, args) -> args)) filterNoVarCts
    
    withTopTyCon :: (Ct, TyCon, [Type]) 
                 -> (TyCon -> SupermonadPluginM s (Maybe ClsInst)) 
                 -> (TyCon -> [Type] -> ClsInst -> SupermonadPluginM s a) 
                 -> SupermonadPluginM s (Either O.SDoc a)
    withTopTyCon (ct, _ctClsTyCon, ctArgs) getTyConInst process = do
      -- Get the top-level type constructor for this bind constraint.
      let mTopTyCon = S.toList $ collectTopTyCons ctArgs
      -- Begin error handling.
      case mTopTyCon of
        [topTyCon] -> do
          -- Get the bind instance for the current type constructor.
          mInst <- getTyConInst topTyCon
          case mInst of
            Just inst -> Right <$> process topTyCon ctArgs inst
            -- We could not find a bind and return instance associated with the 
            -- given type constructor.
            Nothing -> do
              return $ Left
                     $ O.text "Constraints top type constructor does not have an associated instance:"
                       O.$$ O.ppr topTyCon
        _ -> do
          return $ Left
                 $ O.text "Constraint misses a unqiue top-level type constructor:"
                   O.$$ O.ppr ct
    
    deriveUnificationConstraints :: (Ct, TyCon, [Type]) -> ClsInst -> Either O.SDoc ([(Ct, TyVar, Type)], [(Ct, Type, Type)])
    deriveUnificationConstraints (ct, _ctClsTyCon, ctArgs) inst = do
      let (instVars, _instCls, instArgs) = instanceHead inst
      -- let Just ctArgs = constraintClassTyArgs ct
      let ctVars = Set.toList $ Set.unions $ fmap collectTyVars ctArgs
      let mSubsts = zipWith tcUnifyTy instArgs ctArgs
      if any isNothing mSubsts then do
        Left $ O.hang (O.text "Unification constraint solving not possible, because instance and constraint are not unifiable!") 2 
             $ (O.hang (O.text "Instance:") 2 $ O.ppr inst) O.$$
               (O.hang (O.text "Constraint:") 2 $ O.ppr ct) O.$$
               (O.hang (O.text "Constraint arguments:") 2 $ O.ppr ctArgs)
      else do
        let substs = catMaybes mSubsts
        let instVarEqGroups = collectEqualityGroup substs instVars
        instVarEqGroupsCt <- fmap concat $ forM instVarEqGroups $ \(_, eqGroup) -> do
          return $ mkEqGroup ct eqGroup
        -- There may still the type variables from the constraint that were unified
        -- with constants. Also create type equalities for these.
        let ctVarEqGroups = collectEqualityGroup substs $ filter isAmbiguousTyVar ctVars
        let ctVarEqCts = mkEqStarGroup ct ctVarEqGroups
        return (ctVarEqCts, instVarEqGroupsCt)
    
    mkEqGroup ::  Ct -> [Type] -> [(Ct, Type, Type)]
    mkEqGroup _ [] = []
    mkEqGroup baseCt (ty : tys) = fmap (\ty' -> (baseCt, ty, ty')) tys
    
    mkEqStarGroup :: Ct -> [(TyVar, [Type])] -> [(Ct, TyVar, Type)]
    mkEqStarGroup baseCt eqGroups = concatMap (\(tv, tys) -> fmap (\ty -> (baseCt, tv, ty)) tys) eqGroups
    
    collectEqualityGroup :: [TypeVarSubst] -> [TyVar] -> [(TyVar, [Type])]
    collectEqualityGroup substs tvs = [ (tv, nubBy eqType $ filter (all (tv /=) . collectTyVars) 
                                                          $ [ substTyVar subst tv | subst <- substs]
                                        ) | tv <- tvs]
                                        
    prepCt :: TypeVarSubst -> Ct -> Maybe (Ct, TyCon, [Type])
    prepCt subst ct = fmap (\(cls, args) -> (ct, classTyCon cls, substTys subst args)) $ constraintClassType ct
    
    
    
    
    
    