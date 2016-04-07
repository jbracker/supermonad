 
module Control.Supermonad.Plugin.Solving 
  ( solveConstraints
  ) where

import Data.Maybe ( catMaybes, fromJust )
import Data.List ( partition )
import qualified Data.Set as S

import Control.Monad ( when, forM, forM_ )

import TcRnTypes ( Ct )
import TyCon ( TyCon, tyConKind )
import Type 
  ( Type, TyVar, TvSubst
  , mkTopTvSubst
  , substTys )
import Var ( tyVarKind )
import TcPluginM ( TcPluginM )
import TcType ( isAmbiguousTyVar )
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
  , addDerivedResult
  , runTcPlugin
  , printMsg, printConstraints, printObj
  , throwPluginError, catchPluginError
  , assertM )
import Control.Supermonad.Plugin.Constraint 
  ( WantedCt
  , mkDerivedTypeEqCt
  , isClassConstraint
  , constraintClassType )
import Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons, componentTopTcVars )
import Control.Supermonad.Plugin.Evidence 
  ( isPotentiallyInstantiatedCt
  , matchInstanceTyVars' )
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
  
  -- Find a possible association for each of the constraint groups.
  solvedGroups <- fmap catMaybes $ forM ctGroups $ \ctGroup -> do 
    catchPluginError (Just <$> solveConstraintGroup ctGroup) $ \err -> do
      printMsg err
      return Nothing
  
  -- Partially apply type constructors in associations to fit the kind of 
  -- the type variable they are associated with.
  appliedSolvedGroups <- forM solvedGroups $ \(ctGroup, assoc) -> do
    appliedAssoc <- forM assoc $ \(tv, tc) -> do
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
    return (ctGroup, appliedAssoc)
  
  -- For each solved group, try to produde evidence for each involved constraint.
  forM_ appliedSolvedGroups $ \(ctGroup, appliedAssoc) -> do
    forM_ appliedAssoc $ \(tv, ty, _flexVars) -> do
      addDerivedResult $ mkDerivedTypeEqCt (head ctGroup) tv ty
    -- FIXME: Required when we want to handle flexi vars that were introduced while 
    -- applying type constructors partially.
    --produceEvidenceForCts ctGroup appliedAssoc returnCls
    --produceEvidenceForCts ctGroup appliedAssoc bindCls
    
    return ()
  
  -- Unnecessary, just there to mark end of do-block
  return ()
  
  where
    -- | Takes a constraint group together with the association between type 
    --   variables and (partially applied) type constructors and the class of
    --   the constraints to provide evidence for.
    produceEvidenceForCts :: [WantedCt] -> [(TyVar, Type, [TyVar])] -> Class -> SupermonadPluginM TvSubst
    produceEvidenceForCts ctGroup appliedAssocs cls = do
      undefined
      {-
      -- FIXME: For now we do not handle flexivars that were introduced.
      
      -- Create the substitution for the applied associations.
      let assocSubst = mkTopTvSubst $ fmap (\(tv, t, _) -> (tv, t)) appliedAssocs
      
      let allFlexiVars :: S.Set TyVar
          allFlexiVars = S.unions $ fmap (S.fromList . (\(_, _, flexis) -> flexis)) appliedAssocs
      
      -- Find all constraint to provide evidence for
      let cts = filter (isClassConstraint cls) ctGroup
      
      -- Try to provide evidence for each one.
      _ <- forM cts $ \ct -> do
        instEnvs <- getInstEnvs
        -- Instantiate the variables in the constraint with their (partially applied)
        -- type constructor.
        let (_cls, ctArgs) = fromJust $ constraintClassType ct
        let tyArgs = substTys assocSubst ctArgs
        
        let localFlexiVars = S.toList $ S.intersection allFlexiVars $ S.unions $ fmap collectTyVars tyArgs
        -- Lookup an instance for the current constraint.
        -- Although we require to find a unique instance we do not use 
        -- 'lookupUniqueInstEnv', because it does not allow looking up instances
        -- while using flexi vars.
        case lookupInstEnv instEnvs cls tyArgs of
          ([(inst, instArgs)], [], _) -> do
            undefined
          ([], [inst], _) -> do
            let (_instVarTys, flexiVarAssocs) = fromJust $ matchInstanceTyVars' localFlexiVars inst tyArgs
            
            
            undefined
          _ -> do
            printMsg "Found to many instance matches for the following class application:"
            printObj cls
            printObj tyArgs
      -}


solveConstraintGroup :: [WantedCt] -> SupermonadPluginM ([WantedCt], [(TyVar, Either TyCon TyVar)])
solveConstraintGroup [] = throwPluginError "Solving received an empty constraint group!"
solveConstraintGroup ctGroup = do
  -- Get the all of the given constraints.
  givenCts <- getGivenConstraints
  
  let (tyConVars, tyConBaseVars) = partition isAmbiguousTyVar $ componentTopTcVars ctGroup :: ([TyVar], [TyVar])
  
  let tyConBase :: [Either TyCon TyVar]
      tyConBase = fmap Left (componentTopTyCons ctGroup) ++ fmap Right tyConBaseVars
  
  let assocs :: [[(TyVar, Either TyCon TyVar)]]
      assocs = associations $ fmap (\tv -> (tv, tyConBase)) tyConVars
  
  -- Debugging output
  printMsg $ "Solving Group..."
  printMsg $ "Size = " ++ show (length ctGroup) ++ "; "
           ++ "Vars = " ++ show (length tyConVars) ++ "; "
           ++ "Base Size = " ++ show (length tyConBase) ++ "; "
           ++ "Associations = " ++ show (length assocs) ++ ";"
  
  -- For each association check if all constraints are potentially instantiable 
  -- with that association.
  checkedAssocs <- forM assocs $ \assoc -> do
    -- isPotentiallyInstantiatedCt :: [GivenCt] -> Ct -> [(TyVar, TyCon)] -> TcPluginM Bool
    validAssoc <- allM (\ct -> runTcPlugin $ isPotentiallyInstantiatedCt givenCts ct assoc) ctGroup
    return (assoc, validAssoc)
  
  -- Only keep those associations that could be satisfiable. 
  let validAssocs = fmap fst $ filter snd checkedAssocs
  
  -- Output for debugging:
  when (length validAssocs >= 1) $ do
    printMsg "Current Constraint Group:"
    printConstraints ctGroup
    printMsg "Satisfiable associations:"
    forM_ validAssocs printObj
  
  case validAssocs of
    -- If there is only one possible association pick it.
    [assoc] -> return (ctGroup, assoc)
    -- If there are several possible associations this code is ambiguous, stop!
    (a:as) -> do
      printConstraints ctGroup
      throwPluginError "There is more then one possible association for the current constraint group!"
    -- If there is no possible association the code can not be compiled, stop!
    [] -> do
      printConstraints ctGroup
      throwPluginError "There are no possible associations for the current constraint group!"
