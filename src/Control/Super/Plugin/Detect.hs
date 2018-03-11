
-- | Functions and utilities to detect the importent modules, classes
--   and types of the plugin.
module Control.Super.Plugin.Detect
  ( -- * Searching for Modules
    ModuleQuery(..)
  , findModuleByQuery
  , findModule
  , defaultFindEitherModuleErrMsg
    -- * Searching for Classes
  , ClassQuery(..)
  , isOptionalClassQuery
  , queriedClasses, moduleQueryOf
  , findClassesByQuery
  , findClass
  , isClass
    -- * Searching for Instances
  --, findInstancesInScope
  --, findClassAndInstancesInScope
  , findClassesAndInstancesInScope
  , findMonoTopTyConInstances
    -- * Instance implications
  , InstanceImplication
  , (===>), (<==>)
  , clsDictInstImp, clsDictInstEquiv
  , checkInstanceImplications
    -- * Validation
  , checkInstances
  ) where

import Data.List  ( find )
import Data.Either ( isLeft, isRight )
import Data.Maybe ( isNothing, maybeToList, catMaybes, fromMaybe )

import Control.Monad ( forM )

import BasicTypes ( Arity )
import TcRnTypes
  ( TcGblEnv(..)
  , ImportAvails( imp_mods ) )
import TyCon ( TyCon )
import TcPluginM
  ( TcPluginM
  , getEnvs, getInstEnvs )
import Name
  ( nameModule
  , getOccName )
import OccName
  ( occNameString )
--import RdrName
--  ( GlobalRdrElt(..)
--  , Parent( NoParent ) )
import Module
  ( Module, ModuleName
  , moduleName
  , moduleEnvKeys
  , mkModuleName )
import Class
  ( Class(..)
  , className, classArity )
import InstEnv
  ( ClsInst(..)
  , instEnvElts
  , ie_global
  , classInstances )
import PrelNames ( mAIN_NAME )
import Outputable ( SDoc, ($$), text, vcat, ppr, hang )
import qualified Outputable as O

import Control.Super.Plugin.Collection.Set ( Set )
import qualified Control.Super.Plugin.Collection.Set as S
import qualified Control.Super.Plugin.Collection.Map as M
--import Control.Super.Plugin.Log ( printObj, printObjTrace, printMsg )
import Control.Super.Plugin.Wrapper
  ( UnitId, moduleUnitId )
import Control.Super.Plugin.Instance
  ( instanceTopTyCons
  , isMonoTyConInstance 
  , isPolyTyConInstance )
import Control.Super.Plugin.Utils
  ( errIndent
  , removeDupByIndexEq
  , fromRight, fromLeft
  , getClassName, getTyConName )
import Control.Super.Plugin.ClassDict
  ( ClassDict, Optional
  , allClsDictEntries
  , lookupClsDictClass )
import Control.Super.Plugin.InstanceDict
  ( InstanceDict
  , insertInstDict, emptyInstDict
  , allInstDictTyCons
  , lookupInstDictByTyCon )
import Control.Super.Plugin.Names

-- -----------------------------------------------------------------------------
-- Validation
-- -----------------------------------------------------------------------------

-- | Check if there are any supermonad instances that clearly 
--   do not belong to a specific supermonad.
checkInstances
  :: ClassDict
  -- ^ The class dictionary to lookup instances of specific classes.
  -> InstanceDict
  -- ^ The instance dictionary to check for validity.
  --   This should be the instances calculated by 'findMonoTopTyConInstances'
  --   from the given class dict. If not the validity of the checks cannot be
  --   guarenteed.
  -> [InstanceImplication]
  -- ^ The instance implications to check on the given instance dictionary.
  -> [(Either (TyCon, Class) ClsInst, SDoc)]
checkInstances clsDict instDict instImplications = 
  monoCheckErrMsgs ++ polyCheckErrMsgs
  where 
    -- Check if all instances for each supermonad type constructor exist.
    monoCheckErrMsgs :: [(Either (TyCon, Class) ClsInst, SDoc)]
    monoCheckErrMsgs = fmap (\(tc, msg) -> (Left tc, msg)) 
                     $ removeDupByIndexEq
                     $ checkInstanceImplications instDict 
                     $ instImplications
   
    -- Check if there are any instance that involve different type constructors...
    polyCheckErrMsgs :: [(Either (TyCon, Class) ClsInst, SDoc)]
    polyCheckErrMsgs = do
      (_opt, mClsInsts) <- allClsDictEntries clsDict
      case mClsInsts of
        Just (cls, insts) -> do
          polyInst <- filter (isPolyTyConInstance cls) insts
          return (Right polyInst, text "Instance involves more then one top-level type constructor: " $$ ppr polyInst)
        Nothing -> []

-- -----------------------------------------------------------------------------
-- Searching for Modules
-- -----------------------------------------------------------------------------

-- | Formulates queries to find modules.
data ModuleQuery
  = ThisModule PluginModuleName (Maybe UnitId)
  -- ^ Search for a specific module using its name and optionally its 
  --   unit ID (module ID).
  | EitherModule [ModuleQuery] (Maybe ([Either SDoc Module] -> SDoc))
  -- ^ Find either of the modules described by the given subqueries.
  --   If only one of the queries delivers a result, it will be used
  --   otherwise an error will be returned. The error message is customizable
  --   using the optional function.
  | AnyModule    [ModuleQuery]
  -- ^ Find any of the modules described by the given subqueries.
  --   The first one found (in order of the queries) will be delivered as result,
  --   the rest will be ignored. If no module could be found an error message
  --   will be returned.

instance O.Outputable ModuleQuery where
  ppr (ThisModule mdlName mUnitId) = O.text mdlName O.<> (maybe (O.text "") O.ppr $ mUnitId)
  ppr (EitherModule mdlQueries _errF) = O.text "XOR " O.<> (O.brackets $ O.hcat $ O.punctuate (O.text ", ") $ fmap O.ppr mdlQueries)
  ppr (AnyModule mdlQueries) = O.text "OR " O.<> (O.brackets $ O.hcat $ O.punctuate (O.text ", ") $ fmap O.ppr mdlQueries)

collectModuleNames :: ModuleQuery -> Set ModuleName
collectModuleNames (ThisModule name _) = S.singleton $ mkModuleName name
collectModuleNames (EitherModule qs _) = S.unions $ fmap collectModuleNames qs
collectModuleNames (AnyModule qs)      = S.unions $ fmap collectModuleNames qs

isModuleInQuery :: ModuleQuery -> Module -> Bool
isModuleInQuery query mdl = S.member (moduleName mdl) 
                          $ S.insert mAIN_NAME 
                          $ collectModuleNames query

-- | Tries to find a module using the given module query.
findModuleByQuery :: ModuleQuery -> TcPluginM (Either SDoc Module)
findModuleByQuery (ThisModule mdlName mdlUnit) = findModule mdlUnit mdlName
findModuleByQuery (EitherModule queries mErrFun) = do
  queryResults <- forM queries findModuleByQuery
  return $ findEitherModule mErrFun queryResults
findModuleByQuery (AnyModule queries) = do
  queryResults <- forM queries findModuleByQuery
  return $ findAnyModule Nothing queryResults

-- | Checks if the module with the given name is imported and,
--   if so, returns that module.
findModule :: Maybe UnitId -> String -> TcPluginM (Either SDoc Module)
findModule pkgKeyToFind mdlNameToFind = do
  (gblEnv, _lclEnv) <- getEnvs
  let mdls = moduleEnvKeys $ imp_mods $ tcg_imports $ gblEnv
  case find (isModule . splitModule) mdls of
    Just mdl -> return $ Right mdl
    Nothing  -> return $ Left $ text $ "Could not find module '" ++ mdlNameToFind ++ "'"
  where
    isModule :: (UnitId, ModuleName) -> Bool
    isModule (pkgKey, mdlName) 
      =  maybe True (pkgKey ==) pkgKeyToFind 
      && mdlName == mkModuleName mdlNameToFind
    
    splitModule :: Module -> (UnitId, ModuleName)
    splitModule mdl = (moduleUnitId mdl, moduleName mdl)

-- | Makes sure that only one of the given modules was found and returns that
--   found module. If many or none of them were found an error is returned.
--   The returned error message can be customized using the optional 
--   function.
findEitherModule :: Maybe ([Either SDoc Module] -> SDoc) -> [Either SDoc Module] -> (Either SDoc Module)
findEitherModule mErrFun eMdls = 
  case fmap fromRight $ filter isRight eMdls of
    [] -> Left $ fromMaybe defaultFindEitherModuleErrMsg mErrFun $ eMdls
    [mdl] -> Right mdl
    _ -> Left $ fromMaybe defaultFindEitherModuleErrMsg mErrFun $ eMdls

-- | Makes sure that at least one of the given modules was found and, if so, returns 
--   the first one found (in order of the list). If none of the modules 
--   was found an error message will be returned.
--   The returned error message can be customized using the optional 
--   function.
findAnyModule :: Maybe ([SDoc] -> SDoc) -> [Either SDoc Module] -> (Either SDoc Module)
findAnyModule mErrFun eMdls = 
  case fmap fromRight $ filter isRight eMdls of
    [] -> Left $ fromMaybe defaultFindAnyModuleErrMsg mErrFun $ fmap fromLeft eMdls
    (mdl : _) -> Right mdl

-- | Default error message function in case 'EitherModule' fails.
defaultFindEitherModuleErrMsg :: [Either SDoc Module] -> SDoc
defaultFindEitherModuleErrMsg mdls = case found of
    [] -> hang (text "Failed to find either module!") errIndent $ vcat notFound
    _ -> hang (text "Found several modules, unclear which one to use:") errIndent $ vcat $ fmap ppr found
  where
    found = fmap fromRight $ filter isRight mdls
    notFound = fmap fromLeft $ filter isLeft mdls

-- | Default error message function in case 'AnyModule' fails.
defaultFindAnyModuleErrMsg :: [SDoc] -> SDoc
defaultFindAnyModuleErrMsg mdlErrs = hang (text "Could not find any of the modules!") errIndent $ vcat mdlErrs


-- -----------------------------------------------------------------------------
-- Searching for Classes
-- -----------------------------------------------------------------------------

-- | Find a collection of classes in the given module.
data ClassQuery = ClassQuery Optional ModuleQuery [(PluginClassName, Arity)]

instance O.Outputable ClassQuery where
  ppr (ClassQuery opt mdlQuery clsNames)
    = O.hang (O.text "In module:") errIndent (O.ppr mdlQuery) 
    O.<> O.hang (O.text $ (if opt then "optionally " else "") ++ "find classes:") errIndent (O.ppr clsNames)

-- | Check if the classes requested by the given query are optional.
isOptionalClassQuery :: ClassQuery -> Bool
isOptionalClassQuery (ClassQuery opt _mdlQ _clss) = opt

-- | Get the names of the classes that are queried for by the given query.
queriedClasses :: ClassQuery -> [PluginClassName]
queriedClasses (ClassQuery _opt _mdlQ clss) = fmap fst clss

-- | Get the module query this class query uses to lookup modules of classes.
moduleQueryOf :: ClassQuery -> ModuleQuery
moduleQueryOf (ClassQuery _opt mdlQ _clss) = mdlQ

-- | Search for a collection of classes using the given query.
--   If any one of the classes could not be found an error is returned.
findClassesByQuery :: ClassQuery -> TcPluginM (Either SDoc [(PluginClassName, Class)])
findClassesByQuery (ClassQuery opt mdlQuery toFindCls) = do
  eClss <- forM toFindCls $ \(clsName, clsArity) -> do
    eCls <- findClass (isClass (isModuleInQuery mdlQuery) clsName clsArity)
    return (clsName, eCls, clsArity)
  let notFound = filter (\(_, c, _) -> isNothing c) eClss
  let errMsg :: (PluginClassName, Maybe Class, Arity) -> SDoc
      errMsg (n, _, a) = text $ "Could not find class '" ++ n ++ "' with arity " ++ show a ++ "!"
  return $ case notFound of
    -- We found the classes.
    [] -> Right $ fmap (\(n, Just c, _) -> (n, c)) $ eClss
    -- If the classes are optional, we return an empty list of classes.
    _ | opt -> Right []
    -- The classes aren't optional therefore we return an error.
    _ -> Left $ vcat $ fmap errMsg notFound
  
-- | Checks if a type class matching the shape of the given 
--   predicate is in scope.
findClass :: (Class -> Bool) -> TcPluginM (Maybe Class)
findClass isCls' = do
  let isCls = isCls' . is_cls
  envs <- fst <$> getEnvs
  -- This is needed while compiling the package itself...
  let foundInstsLcl =  (filter isCls . instEnvElts . tcg_inst_env $ envs)
                    ++ (filter isCls . tcg_insts $ envs)
  -- This is needed while compiling an external package depending on it...
  foundInstsGbl <- filter isCls . instEnvElts . ie_global <$> getInstEnvs
  return $ case foundInstsLcl ++ foundInstsGbl of
    (inst : _) -> Just $ is_cls inst
    [] -> Nothing

-- | Check if the given class has the given name, arity and if the classes
--   module fulfills the given predicate.
isClass :: (Module -> Bool) -> String -> Arity -> Class -> Bool
isClass isModule targetClassName targetArity cls =
  let clsName = className cls
      clsMdl = nameModule clsName
      clsNameStr = occNameString $ getOccName clsName
      clsArity = classArity cls
  in    isModule clsMdl
     && clsNameStr == targetClassName
     && clsArity == targetArity


-- -----------------------------------------------------------------------------
-- Searching for Instances
-- -----------------------------------------------------------------------------

-- | Use a class query to find classes and their instances.
findClassesAndInstancesInScope :: ClassQuery -> TcPluginM (Either SDoc [(PluginClassName, Class, [ClsInst])])
findClassesAndInstancesInScope clsQuery = do
  eClss <- findClassesByQuery clsQuery
  case eClss of
    Left err -> return $ Left err
    Right clss -> fmap Right $ forM clss $ \(n, c) -> do
      insts <- findInstancesInScope c
      return (n, c, insts)

-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls

-- | Constructs the map between type constructors and their supermonad instances.
--   It essentially collects all of the instances that only use a single top-level
--   constructor and stores them in the instance dictionary. If there are several
--   instances for a single type constructor none is added to the dictionary.
--   This function only searches for the instances and constructs the lookup table.
findMonoTopTyConInstances
  :: ClassDict
  -- ^ The set of classes and instances to calculate the instance dictionary from.
  -> InstanceDict
  -- ^ Association between type constructors and their supermonad instances.
findMonoTopTyConInstances clsDict =
  mconcat $ do
    tc <- supermonadTyCons
    (cls, insts) <- dictEntries
    return $ findMonoClassInstance tc cls insts
  where
    dictEntries :: [(Class, [ClsInst])]
    dictEntries = catMaybes $ fmap snd $ allClsDictEntries clsDict
    
    -- Collect all type constructors that are used for supermonads
    supermonadTyCons :: [TyCon]
    supermonadTyCons = S.toList 
                       $ S.unions
                       $ fmap instanceTopTyCons
                       $ concat $ fmap snd dictEntries
    
    findMonoClassInstance :: TyCon -> Class -> [ClsInst] -> InstanceDict
    findMonoClassInstance tc cls insts = 
      case filter (isMonoTyConInstance tc cls) insts of
        [foundInst] -> insertInstDict tc cls foundInst $ emptyInstDict
        _ -> emptyInstDict

-- -----------------------------------------------------------------------------
-- Instance implications
-- -----------------------------------------------------------------------------

-- | Representation of instance implying the existence of other instances that
--   are using the same type constructor.
data InstanceImplication = InstanceImplies Class Class

instance O.Outputable InstanceImplication where
  ppr (InstanceImplies ca cb) = O.text (getClassName ca) O.<> O.text " ===> " O.<> O.text (getClassName cb)

infix 7 ===>
infix 7 <==>

-- | Instance of first class implies the existence of an instance from the second class.
(===>) :: Class -> Class -> [InstanceImplication]
(===>) ca cb = [InstanceImplies ca cb]

-- | Instances of either class imply the respective other instance.
(<==>) :: Class -> Class -> [InstanceImplication]
(<==>) ca cb = ca ===> cb ++ cb ===> ca

-- | Instance implication based on lookup of names in class dictionary. See '===>'.
clsDictInstImp :: ClassDict -> PluginClassName -> PluginClassName -> [InstanceImplication]
clsDictInstImp clsDict caName cbName = do
  clsA <- maybeToList $ lookupClsDictClass caName clsDict
  clsB <- maybeToList $ lookupClsDictClass cbName clsDict
  clsA ===> clsB

-- | Instance equivalence based on lookup of names in class dictionary. See '<==>'.
clsDictInstEquiv :: ClassDict -> PluginClassName -> PluginClassName -> [InstanceImplication]
clsDictInstEquiv clsDict caName cbName = do
  clsA <- maybeToList $ lookupClsDictClass caName clsDict
  clsB <- maybeToList $ lookupClsDictClass cbName clsDict
  clsA <==> clsB

-- | Check a given instance dictionary against a set of 'InstanceImplication's 
--   and return error messages if there are violations.
checkInstanceImplications :: InstanceDict -> [InstanceImplication] -> [((TyCon,Class), SDoc)]
checkInstanceImplications _instDict [] = []
checkInstanceImplications instDict (imp : imps) = do
  tc <- S.toList $ allInstDictTyCons instDict
  let tcDict = lookupInstDictByTyCon tc instDict
  case imp of
    InstanceImplies ca cb -> case (M.member ca tcDict, M.member cb tcDict) of
      (False, _    ) -> rest
      (True , True ) -> rest
      (True , False) -> 
        let errMsg = text $ "There is no unique instance of '" ++ getClassName cb ++ "' for the type '" ++ getTyConName tc ++ "'!"
        in ((tc,cb), errMsg) : rest
  where
    rest = checkInstanceImplications instDict imps



