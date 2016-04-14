
-- | Provides functions to check if a given instance together with some
--   arguments to instantiate it, is actually instantiated by those arguments.
--   Also provides functions to produce evidence for the instantiations if
--   necessary.
module Control.Supermonad.Plugin.Evidence
  ( matchInstanceTyVars
  , matchInstanceTyVars'
  , isPotentiallyInstantiatedCt
  , produceEvidenceFor
  , produceEvidenceForCt
  ) where

import Data.Either ( isLeft )
import Data.List ( find )
import qualified Data.Set as S

import Control.Monad ( forM )

import Type
  ( Type, TyVar
  , mkTopTvSubst, mkTyVarTy, mkAppTys, mkTyConTy
  , substTy, substTys
  , eqType
  , getClassPredTys_maybe, getClassPredTys
  , getEqPredTys_maybe, getEqPredTys, getEqPredRole
  , getTyVar_maybe
  , splitTyConApp_maybe, splitFunTy_maybe, splitAppTy_maybe )
import Class ( classTyCon )
import TyCon
  ( TyCon
  , isTupleTyCon, isTypeFamilyTyCon, isTypeSynonymTyCon )
import Class ( Class )
import Coercion ( Coercion(..) )
import CoAxiom ( Role(..) )
import InstEnv
  ( ClsInst(..)
  , instanceSig
  , lookupInstEnv
  , lookupUniqueInstEnv )
import Unify ( tcUnifyTys )
import TcRnTypes ( Ct, isGivenCt, ctPred, ctEvidence, ctEvTerm )
import TcType ( isAmbiguousTyVar )
import TcEvidence ( EvTerm(..), TcCoercion(..) )
import TcPluginM
  ( TcPluginM
  , getInstEnvs, getFamInstEnvs )
import FamInstEnv ( normaliseType )
import Outputable ( ($$), SDoc )
import qualified Outputable as O

import Control.Supermonad.Plugin.Constraint 
  ( GivenCt
  , constraintClassType
  , constraintPredicateType )
import Control.Supermonad.Plugin.Log ( pluginFailSDoc )
import Control.Supermonad.Plugin.Utils
  ( fromLeft, fromRight
  , collectTyVars
  --, applyTyCon
  , partiallyApplyTyCons
  , allM
  , skolemVarsBindFun )
import Control.Supermonad.Plugin.Detect 
  ( areBindApplyArguments, areBindFunctorArguments )
--import qualified Control.Supermonad.Plugin.Log as L
--import qualified Control.Supermonad.Plugin.Debug as D



-- | Try to evaluate the given type as far as possible by evaluating contained
--   type families and expanding type synonyms.
evaluateType :: Role -> Type -> TcPluginM (Coercion, Type)
evaluateType r t = do
  famInstEnvs <- getFamInstEnvs
  return $ normaliseType famInstEnvs r t

-- | Trys to see if the given arguments match the class instance
--   arguments by unification. This only works if the number of arguments
--   given is equal to the arguments taken by the class the instance is of.
--   If the given arguments match the class arguments, a list with a type for
--   each free variable in the instance is returned. This list is in the same
--   order as the list of free variables that can be retrieved from the instance.
--   
--   This function is meant for use in conjunction with 'isInstanceOf',
--   'isInstantiatedBy' and 'produceEvidenceFor'.
--   
--   The second element of the pair shows how each ambiguous type variable in 
--   the given list type arguments would be bound by the instance.
matchInstanceTyVars :: ClsInst -> [Type] -> Maybe ([Type], [(TyVar, Type)])
matchInstanceTyVars inst instArgs = do
  let ambVars = S.toList $ S.filter isAmbiguousTyVar $ S.unions $ fmap collectTyVars instArgs
  matchInstanceTyVars' ambVars inst instArgs

-- | Trys to see if the given arguments match the class instance
--   arguments by unification. This only works if the number of arguments
--   given is equal to the arguments taken by the class the instance is of.
--   If the given arguments match the class arguments, a list with a type for
--   each free variable in the instance is returned. This list is in the same
--   order as the list of free variables that can be retrieved from the instance.
--   
--   The first argument gives those variables that should be bound and their substitution 
--   be returned.
--   
--   The second element of the pair shows how each given type variable in 
--   the list type argumproduceEvidenceForCtents would be bound by the instance.
matchInstanceTyVars' :: [TyVar] -> ClsInst -> [Type] -> Maybe ([Type], [(TyVar, Type)])
matchInstanceTyVars' varsToBind inst instArgs = do
  let (instVars, _cts, _cls, tyArgs) = instanceSig inst
  let constVars = S.toList $ (S.unions $ fmap collectTyVars instArgs) S.\\ (S.fromList varsToBind)
  subst <- tcUnifyTys (skolemVarsBindFun constVars) tyArgs instArgs
  return $ (substTy subst . mkTyVarTy <$> instVars, fmap (\v -> (v, substTy subst $ mkTyVarTy v)) varsToBind)

-- | Apply the given instance dictionary to the given type arguments
--   and try to produce evidence for the application.
--
--   The list of types has to match the number of open variables of the
--   given instance dictionary in length. They need to match up with
--   the list of free type variables given for the class instance ('is_tvs').
--   The list can be created using 'matchInstanceTyVars'.
--   
--   The first argument is the 'Bind' class. The second and thrid argument
--   is the functor and apply 'Bind' instance respectivly.
--   
--   The fourth argument is a list of given constraints that can be used
--   to produce evidence for otherwise not fulfilled constraints. Be aware that
--   only actually /given/ constraints (as in 'isGivenCt') are useful here,
--   because only those produce evidence for themselves. All other constraints
--   will be ignored.
--
--   This function should properly work with type synonyms and type functions.
--   It only produces evidence for type equalities if they are trivial, i.e., @a ~ a@.
--   
--   Handles 'Bind' constraints that can be resolved with the functor or 
--   apply instance specially. This leads to 'Bind Identity Identity Identity'
--   being solved although there is no unique matching instance for it.
produceEvidenceFor :: Class -- ^ The 'Bind' class.
                   -> ClsInst -- ^ The functor 'Bind' instance.
                   -> ClsInst -- ^ The apply 'Bind' instance.
                   -> TyCon -- ^ The 'Identity' type constructor.
                   -> [GivenCt] -- ^ The given constraints to be used when producing evidence.
                   -> ClsInst -> [Type] -> TcPluginM (Either SDoc EvTerm)
produceEvidenceFor bindCls instFunctor instApply idTyCon givenCts inst instArgs = do
  -- Get the instance type variables and constraints (by that we know the
  -- number of type arguments and dictionart arguments for the EvDFunApp)
  let (tyVars, instCts, _cls, _tyArgs) = instanceSig inst -- ([TyVar], [Type], Class, [Type])
  -- How the instance variables for the current instance are bound.
  let varSubst = mkTopTvSubst $ zip tyVars instArgs
  -- Now go over each constraint and find a suitable instance and evidence.
  -- Don't forget to substitute all variables for their actual values,
  ctEvTerms <- forM (substTys varSubst instCts) $ produceEvidenceForCtType bindCls instFunctor instApply idTyCon givenCts
  -- If we found a good instance and evidence for every constraint,
  -- we can create the evidence for this instance.
  return $ if any isLeft ctEvTerms
    then Left
      $ O.text "Can't produce evidence for instance:"
      $$ O.ppr inst
      $$ O.text "Reason:"
      $$ O.vcat (fromLeft <$> filter isLeft ctEvTerms)
    else Right $ EvDFunApp (is_dfun inst) instArgs (fromRight <$> ctEvTerms)

-- | Try to find evidence that proves the given constraint. Only works 
--   properly if there is only one instance matching the given constraint
--   and if the constraint does not contain ambiguous variables.
--   
--   Handles 'Bind' constraints that can be resolved with the functor or 
--   apply instance specially. This leads to 'Bind Identity Identity Identity'
--   being solved although there is no unique matching instance for it.
produceEvidenceForCt :: Class -- ^ The 'Bind' class.
                     -> ClsInst -- ^ The functor 'Bind' instance.
                     -> ClsInst -- ^ The apply 'Bind' instance.
                     -> TyCon -- ^ The 'Identity' type constructor.
                     -> [GivenCt] -- ^ The given constraints to be used when producing evidence.
                     -> Ct -- ^ The constraint to produce evidence for.
                     -> TcPluginM (Either SDoc EvTerm)
produceEvidenceForCt bindCls instFunctor instApply idTyCon givenCts ct =
  produceEvidenceForCtType bindCls instFunctor instApply idTyCon givenCts $ constraintPredicateType ct


-- | Try to find evidence that proves the given constraint given by the type 
--   representing the constraint predicate. Only works 
--   properly if there is only one instance matching the given constraint
--   and if the constraint does not contain ambiguous variables.
--   
--   Handles 'Bind' constraints that can be resolved with the functor or 
--   apply instance specially. This leads to 'Bind Identity Identity Identity'
--   being solved although there is no unique matching instance for it.
produceEvidenceForCtType :: Class -- ^ The 'Bind' class.
                         -> ClsInst -- ^ The functor 'Bind' instance.
                         -> ClsInst -- ^ The apply 'Bind' instance.
                         -> TyCon -- ^ The 'Identity' type constructor.
                         -> [GivenCt] -- ^ The given constraints to be used when producing evidence.
                         -> Type -- ^ The constraint to produce evidence for.
                         -> TcPluginM (Either SDoc EvTerm)
produceEvidenceForCtType bindCls instFunctor instApply idTyCon givenCts ct =
  case splitTyConApp_maybe ct of
    -- Do we have a tuple of constraints?
    Just (tc, tcArgs) | isTupleTyCon tc -> do
      -- Produce evidence for each element of the tuple
      tupleEvs <- mapM produceEvidenceForCtType' tcArgs
      return $ if any isLeft tupleEvs
        then Left
          $ O.text "Can't find evidence for this tuple constraint:"
          $$ O.ppr ct
          $$ O.text "Reason:"
          $$ O.vcat (fromLeft <$> filter isLeft tupleEvs)
        -- And put together evidence for the complete tuple.
        else Right $ EvTupleMk $ fmap fromRight tupleEvs
    -- Do we have a type family application?
    Just (tc, _tcArgs) | isTyFunCon tc -> do
      -- Evaluate it...
      (coer, evalCt) <- evaluateType Representational ct
      -- Produce evidence for the evaluated term
      eEvEvalCt <- produceEvidenceForCtType' evalCt
      -- Add the appropriate cast to the produced evidence
      return $ (\ev -> EvCast ev (TcSymCo $ TcCoercion coer)) <$> eEvEvalCt
    -- Do we have a type equality constraint?
    _ -> case getEqPredTys_maybe ct of
      -- If there is a synonym or type function in the equality...
      Just _ | containsTyFunApp ct -> do
          -- Evaluate it...
          (coer, evalCt) <- evaluateType Representational ct
          -- Produce evidence for the evaluated term and
          -- add the appropriate cast to the produced evidence
          let (ta, tb) = getEqPredTys evalCt
          let r = getEqPredRole evalCt
          return $ (\ev -> EvCast ev (TcSymCo $ TcCoercion coer)) <$> produceTypeEqEv r ta tb
      -- If there isn't we can just proceed...
      Just (r, ta, tb) -> return $ produceTypeEqEv r ta tb
      -- Do we have a class constraint?
      _ -> case getClassPredTys_maybe ct of
        -- If the class constraint contains a type function application...
        Just _ | containsTyFunApp ct -> do
          -- Evaluate it...
          (coer, evalCt) <- evaluateType Representational ct
          -- Produce evidence for the evaluated term and
          -- add the appropriate cast to the produced evidence
          let (cls, args) = getClassPredTys evalCt
          res <- fmap (\ev -> EvCast ev (TcSymCo $ TcCoercion coer)) <$> produceClassCtEv cls args
          -- If this failed there may still be a given constraint that matches...
          case res of
            Right _ -> return res
            Left _ -> do
              newRes <- return $ (\ev -> EvCast ev (TcCoercion coer)) <$> produceGivenCtEv evalCt
              case newRes of
                Right _ -> return newRes
                Left _ -> return res
        -- The class constraint is a functor 'Bind' constraint...
        Just (cls, ctArgs) | isFunctorBind cls ctArgs -> do
          -- This assumes that the functor instance has the following head 
          -- "(Functor m) => Bind m Identity m" and therefore there is only one variable.
          produceEvidenceFor' instFunctor [last ctArgs]
        -- The class constraint is an apply 'Bind' constraint...
        Just (cls, ctArgs) | isApplyBind cls ctArgs -> do
          -- This assumes that the functor instance has the following head 
          -- "(Functor m) => Bind Identity m m" and therefore there is only one variable.
          produceEvidenceFor' instApply [last ctArgs]
        -- It is a normal class constraint...
        Just (ctCls, ctArgs) -> do
          res <- produceClassCtEv ctCls ctArgs
          -- If this failed there may still be a given constraint that matches...
          case res of
            Right _ -> return res
            Left _ -> do
              newRes <- return $ produceGivenCtEv ct
              case newRes of
                Right _ -> return newRes
                Left _ -> return res
        -- In any other case, lets try if one of the given constraints can help...
        _ | containsTyFunApp ct -> do
          -- Evaluate it...
          (coer, evalCt) <- evaluateType Representational ct
          -- and produce the appropriate cast
          return $ (\ev -> EvCast ev (TcCoercion coer)) <$> produceGivenCtEv evalCt
        -- In any other case, lets try if one of the given constraints can help...
        _ -> return $ produceGivenCtEv ct
  where
    produceEvidenceForCtType' :: Type -> TcPluginM (Either SDoc EvTerm)
    produceEvidenceForCtType' = produceEvidenceForCtType bindCls instFunctor instApply idTyCon checkedGivenCts
    
    produceEvidenceFor' :: ClsInst -> [Type] -> TcPluginM (Either SDoc EvTerm)
    produceEvidenceFor' = produceEvidenceFor bindCls instFunctor instApply idTyCon checkedGivenCts
    
    -- Ensure there are only given constraints there.
    checkedGivenCts = filter isGivenCt givenCts

    -- We only do the simplest kind of equality constraint solving and
    -- evidence construction.
    produceTypeEqEv :: Role -> Type -> Type -> Either SDoc EvTerm
    produceTypeEqEv r ta tb = if eqType ta tb
      then Right $ EvCoercion $ TcRefl r ta
      else Left
        $ O.text "Can't produce evidence for this type equality constraint:"
        $$ O.ppr ct

    -- Produce evidence of a class constraint.
    produceClassCtEv :: Class -> [Type] -> TcPluginM (Either SDoc EvTerm)
    produceClassCtEv cls args = do
      -- Get global instance environment
      instEnvs <- getInstEnvs
      -- Look for suitable instance. Since we are not necessarily working
      -- with polymonads anymore we need to find a unique one.
      case lookupUniqueInstEnv instEnvs cls args of -- (snd <$> normCtArgs)
        -- No instance found, too bad...
        Left err ->
          return $ Left
            $ O.text "Can't produce evidence for this class constraint:"
            $$ O.ppr ct
            $$ O.text "Lookup error:"
            $$ err
        -- We found one: Now we can produce evidence for the found instance.
        Right (clsInst, instArgs) -> produceEvidenceFor' clsInst instArgs

    -- Try to find a given constraint that matches and use its evidence.
    produceGivenCtEv :: Type -> Either SDoc EvTerm
    produceGivenCtEv cnstrnt = case find (eqType cnstrnt . ctPred) checkedGivenCts of
      -- Check if there is some given constraint that provides evidence
      -- for our constraint.
      Just foundGivenCt -> Right $ ctEvTerm (ctEvidence foundGivenCt)
      -- Nothing delivered a result, give up...
      Nothing -> Left
        $ O.text "Can't produce evidence for this constraint:"
        $$ O.ppr cnstrnt

    -- Is this type constructor something that requires evaluation?
    isTyFunCon :: TyCon -> Bool
    isTyFunCon tc = isTypeFamilyTyCon tc || isTypeSynonymTyCon tc

    -- | Check of the given type is the application of a type family data constructor.
    isTyFunApp :: Type -> Bool
    isTyFunApp t = case splitTyConApp_maybe t of
      Just (tc, _args) -> isTyFunCon tc
      Nothing -> False

    -- | Find out if there is a type function application somewhere inside the type.
    containsTyFunApp :: Type -> Bool
    containsTyFunApp t = isTyFunApp t ||
      case getTyVar_maybe t of
        Just _tv -> False
        Nothing -> case splitTyConApp_maybe t of
          Just (_tc, args) -> any containsTyFunApp args
          Nothing -> case splitFunTy_maybe t of
            Just (ta, tb) -> containsTyFunApp ta || containsTyFunApp tb
            Nothing -> case splitAppTy_maybe t of
              Just (ta, tb) -> containsTyFunApp ta || containsTyFunApp tb
              Nothing -> case getEqPredTys_maybe t of
                Just (_r, ta, tb) -> containsTyFunApp ta || containsTyFunApp tb
                Nothing -> False
    
    isFunctorBind :: Class -> [Type] -> Bool
    isFunctorBind cls [t0, t1, t2] 
      = cls == bindCls && areBindFunctorArguments idTyCon t0 t1 t2
    isFunctorBind _ _ = False

    isApplyBind :: Class -> [Type] -> Bool
    isApplyBind cls [t0, t1, t2] 
      = cls == bindCls && areBindApplyArguments idTyCon t0 t1 t2
    isApplyBind _ _ = False

-- | Check if a given class constraint can 
--   potentially be instantiated using the given
--   type constructors. By potentially we mean: First, check if the
--   polymonad instance can actually be applied to some combination of
--   the type constructors. Then check if all resulting constraints that
--   do not contain free variables actually can be instantiated.
--   
--   Note: A constraint is only potentially instantiable if there is only
--   one matching instance for the constraint and all of its implied constraints.
--   
--   Note: This function only handle class constraints. It will always deliver 
--   'False' when another constraint type is given.
isPotentiallyInstantiatedCt :: Class -- ^ The 'Bind' class.
                            -> ClsInst -- ^ The functor 'Bind' instance.
                            -> ClsInst -- ^ The apply 'Bind' instance.
                            -> TyCon -- ^ The 'Identity' type constructor.
                            -> [GivenCt] -- ^ The given constraints to be used when producing evidence.
                            -> Ct 
                            -> [(TyVar, Either TyCon TyVar)] 
                            -> TcPluginM Bool
isPotentiallyInstantiatedCt bindCls instFunctor instApply idTyCon givenCts ct assocs = 
  case constraintClassType ct of
    -- If we have a class constraint...
    Just splitCt -> isPotentiallyInstantiatedCtType bindCls instFunctor instApply idTyCon givenCts splitCt assocs
    Nothing -> return False

-- | Utility helper for 'isPotentiallyInstantiatedCt' that checks class constraints.
isPotentiallyInstantiatedCtType :: Class -- ^ The 'Bind' class.
                                -> ClsInst -- ^ The functor 'Bind' instance.
                                -> ClsInst -- ^ The apply 'Bind' instance.
                                -> TyCon -- ^ The 'Identity' type constructor.
                                -> [GivenCt] -- ^ The given constraints to be used when producing evidence.
                                -> (Class, [Type]) 
                                -> [(TyVar, Either TyCon TyVar)] 
                                -> TcPluginM Bool
isPotentiallyInstantiatedCtType bindCls instFunctor instApply idTyCon givenCts (ctCls, ctArgs) assocs = do
  let produceEvidence :: Type -> TcPluginM (Either SDoc EvTerm)
      produceEvidence = produceEvidenceForCtType bindCls instFunctor instApply idTyCon givenCts
  
  -- Get the type constructors partially applied to some new variables as
  -- is necessary.
  appliedAssocs <- either pluginFailSDoc return =<< partiallyApplyTyCons assocs
  
  -- Create the substitution for the given associations.
  let ctSubst = mkTopTvSubst $ fmap (\(tv, t, _) -> (tv, t)) appliedAssocs
  -- Substitute variables in the constraint arguments with the type constructors.
  let ctSubstArgs = substTys ctSubst ctArgs
  -- Calculate set of generated type variables in constraints
  let ctGenVars = S.unions $ fmap (\(_tv, _tcTy, vars) -> S.fromList vars) appliedAssocs
  
  -- If there are no ambiguous or generated type variables in the substituted arguments of our constraint
  -- we can simply check if there is evidence.
  if all (not . containsGivenOrAmbiguousTyVar ctGenVars) ctSubstArgs
    then do
      eEv <- produceEvidence $ mkAppTys (mkTyConTy $ classTyCon ctCls) ctSubstArgs
      return $ either (const False) (const True) eEv
    else do
      instEnvs <- getInstEnvs
      let (instMatches, unificationMatches, _) = lookupInstEnv instEnvs ctCls ctSubstArgs
      -- FIXME: for now we only accept our constraint as potentially 
      -- instantiated iff there is only one match.
      if length instMatches + length unificationMatches == 1 
        then
          -- Look at the found instances and check if their implied constraints 
          -- are also potentially instantiable.
          flip allM (fmap fst instMatches ++ unificationMatches) $ \inst -> do
            -- Match the instance variables so we can check the implied constraints.
            case matchInstanceTyVars inst ctSubstArgs of
              Just (instArgs, _) -> do
                let (instVars, instCtTys, _, _) = instanceSig inst
                let instSubst = mkTopTvSubst $ zip instVars instArgs
                let instSubstCtTys = substTys instSubst instCtTys
                flip allM instSubstCtTys $ \substCtTy -> do
                  if not (containsGivenOrAmbiguousTyVar ctGenVars substCtTy) 
                    -- The implied constraint does not contain generated or
                    -- ambiguous type variable, which means we can actually 
                    -- check its satisfiability.
                    then do
                      eEv <- produceEvidence substCtTy 
                      return $ either (const False) (const True) eEv
                    -- The implied constraint contains generated or ambiguous
                    -- type variables, which means we can't check it, but it
                    -- may potentially be satisfiable.
                    else return True
              -- There was no match, this should never happen.
              Nothing -> return False
        -- We found more then one matching instance for this constraint 
        else return False
  where
    -- Checks if the given type constains any ambiguous variables or if 
    -- it contains any of the given type variables.
    containsGivenOrAmbiguousTyVar :: S.Set TyVar -> Type -> Bool
    containsGivenOrAmbiguousTyVar givenTvs ty = 
      let tyVars = collectTyVars ty
      in any isAmbiguousTyVar tyVars || not (S.null (S.intersection givenTvs tyVars))
  
  
  
  