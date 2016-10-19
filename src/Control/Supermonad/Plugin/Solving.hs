
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

import Control.Monad ( forM, forM_, filterM, liftM2 )

import TcRnTypes ( Ct(..) )
import TyCon ( TyCon )
import Class ( classTyCon )
import Type 
  ( Type, TyVar
  , substTyVar, substTys
  , eqType )
import TcType ( isAmbiguousTyVar )
import InstEnv ( ClsInst, instanceHead )
import Unify ( tcUnifyTy )
import qualified Outputable as O


import Control.Supermonad.Plugin.Debug ( sDocToStr )
import Control.Supermonad.Plugin.Dict ( SupermonadDict )
import Control.Supermonad.Plugin.Wrapper 
  ( TypeVarSubst, mkTypeVarSubst )
import Control.Supermonad.Plugin.Environment 
  ( SupermonadPluginM
  , getGivenConstraints, getWantedConstraints
  , getReturnClass, getBindClass
  , getSupermonadBindFor, getSupermonadReturnFor
  , addTyVarEquality, addTyVarEqualities
  , addTypeEqualities
  , getTyVarEqualities
  , printMsg, printObj, printErr --, printConstraints
  , addWarning, displayWarnings
  , throwPluginError, throwPluginErrorSDoc )
import Control.Supermonad.Plugin.Environment.Lift
  ( isPotentiallyInstantiatedCt
  , isBindConstraint, isReturnConstraint
  , partiallyApplyTyCons )
import Control.Supermonad.Plugin.Constraint 
  ( WantedCt
  , isClassConstraint
  , constraintClassType
  , constraintClassTyArgs )
import Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons, componentTopTcVars
  , componentMonoTyCon )
import Control.Supermonad.Plugin.Utils 
  ( collectTopTcVars
  , collectTopTyCons
  , collectTyVars
  , associations
  , allM )
import Control.Supermonad.Plugin.Log 
  ( formatConstraint )

  
-- | Attempts to solve the given /wanted/ constraints.
solveConstraints :: [WantedCt] -> SupermonadPluginM SupermonadDict ()
solveConstraints wantedCts = do
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
  
  -- We go through all constraints that have their ambiguous 
  -- top-level type constructors variables solved (they are actual type 
  -- constructor (variable)) and solve their indices by unification with
  -- their 'Bind' and 'Return' instances.
  solveSolvedTyConIndices
  
  -- Finally, display warnings if no progress could be made.
  displayWarnings



-- | Solves constraint groups that only involve exactly one supermonad type 
--   constructor. This is done by associating every ambiguous 
--   supermonad type constructor variable with that type constructor.
--   
--   If necessary the associated type constructors are partially applied 
--   to new ambiguous variables to match the kind of type variable they are 
--   associated with.
solveMonoConstraintGroup :: (TyCon, [WantedCt]) -> SupermonadPluginM s ()
solveMonoConstraintGroup (_, []) = return ()
solveMonoConstraintGroup (tyCon, ctGroup) = do
  printMsg "Solve mono group..."
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
solvePolyConstraintGroup :: [WantedCt] -> SupermonadPluginM s ()
solvePolyConstraintGroup ctGroup = do
  printMsg "Solve poly group..."
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
    --    is either unsolvable or we need to let the type checker make
    --    progress using the other solved groups. Therefore, we only queue
    --    a warning in this case in the hope that the next iteration 
    --    of the type checker resolves the issue.
    (_, []) -> do
      topTcVars <- concat <$> mapM collectBindReturnTopTcVars ctGroup
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
    collectBindReturnTopTcVars :: Ct -> SupermonadPluginM s [TyVar]
    collectBindReturnTopTcVars ct = do
      isBindOrReturn <- liftM2 (||) (isBindConstraint ct) (isReturnConstraint ct)
      case (isBindOrReturn, constraintClassType ct) of
        (True, Just (_cls, tyArgs)) -> return $ S.toList $ collectTopTcVars tyArgs
        _ -> return []



-- | Given a constraint group this calculates the set of type constructors
--   involved with that constraint group and then generates all potentially
--   satisfiable (see 'isPotentiallyInstantiatedCt') associations between 
--   ambiguous supermonad type variables and type constructor (variables).
determineValidConstraintGroupAssocs :: [WantedCt] -> SupermonadPluginM s ([WantedCt], [[(TyVar, Either TyCon TyVar)]])
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
    -- | Only keep supermonad constraints ('Bind' and 'Return').
    filterSupermonadCts :: [Ct] -> SupermonadPluginM s [Ct]
    filterSupermonadCts cts = do
      returnCls <- getReturnClass
      bindCls <- getBindClass
      return $ filter (\ct -> isClassConstraint returnCls ct || isClassConstraint bindCls ct) cts

    -- | Filters all supermonad constraints ('Bind' and 'Return')
    --   that contain at least one of the given variables/type constructors
    --   in their arguments.
    filterSupermonadCtsWith :: [Ct] -> S.Set (Either TyCon TyVar) -> SupermonadPluginM s [Ct]
    filterSupermonadCtsWith allCts baseTyCons = do
      cts <- filterSupermonadCts allCts
      filterM predicate cts
      where 
        predicate :: Ct -> SupermonadPluginM s Bool
        predicate ct = do
          ctBase <- getTyConBaseFrom [ct]
          return $ not $ S.null $ S.intersection ctBase baseTyCons
    
    -- | Calculate the type constructor base for the given constraint.
    getTyConBaseFrom :: [Ct] -> SupermonadPluginM s (S.Set (Either TyCon TyVar))
    getTyConBaseFrom cts = do
      checkedCts <- filterSupermonadCts cts
      let baseTvs :: S.Set (Either TyCon TyVar)
          baseTvs = S.map Right $ S.filter (not . isAmbiguousTyVar) $ componentTopTcVars checkedCts
      let baseTcs :: S.Set (Either TyCon TyVar)
          baseTcs = S.map Left $ componentTopTyCons checkedCts
      return $ S.union baseTvs baseTcs
    
    -- | Collect all ambiguous top-level type constructor variables
    --   from the given constraints.
    getAmbTyConVarsFrom :: [Ct] -> SupermonadPluginM s (S.Set TyVar)
    getAmbTyConVarsFrom cts = do
      checkedCts <- filterSupermonadCts cts
      return $ S.filter isAmbiguousTyVar $ componentTopTcVars checkedCts



-- | Solves the indices of constraints that have been associated a type 
--   constructor.
solveSolvedTyConIndices :: SupermonadPluginM SupermonadDict ()
solveSolvedTyConIndices = do
  -- Get the type equalities that were determined up until now 
  -- and create a substitution from them.
  tyVarEqs <- getTyVarEqualities
  let tvSubst = mkTypeVarSubst $ fmap (\(_ct, tv, ty) -> (tv, ty)) tyVarEqs
  
  -- Get all of the wanted constraints and prepare them for processing
  -- by spliting out there arguments and substiuting the already determined 
  -- type variable equalities.
  wantedCts <- getWantedConstraints
  let prepWantedCts = catMaybes $ fmap (prepCt tvSubst) wantedCts
  
  -- Unification solve return constraints that are applied to top-level tycons.
  printMsg "Unification solve return constraints..."
  unificationSolve prepWantedCts isReturnConstraint (\tc -> getSupermonadReturnFor tc)
  
  -- Unification solve bind constraints that are applied to top-level tycons.
  printMsg "Unification solve bind constraints..."
  unificationSolve prepWantedCts isBindConstraint (\tc -> getSupermonadBindFor tc)
  
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
      let filterNoVarCts = filter (\(_ct, _tc, args) -> not $ S.null 
                                                            $ S.filter isAmbiguousTyVar 
                                                            $ S.unions 
                                                            $ fmap collectTyVars args) 
                                  predFilteredCts
      -- Get all constraints that already have been assigned their top-level
      -- type constructors and process them to solve the type constructor arguments...
      return $ filter (S.null . collectTopTcVars . (\(_ct, _tc, args) -> args)) filterNoVarCts
    
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
      let ctVars = S.toList $ S.unions $ fmap collectTyVars ctArgs
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
    
    
    
    
    
    