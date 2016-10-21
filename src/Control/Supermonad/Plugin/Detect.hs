
-- | Functions and utilities to detect the importent modules, classes
--   and types of the plugin.
module Control.Supermonad.Plugin.Detect
  ( -- * Searching for Modules
    ModuleQuery(..)
  , findModuleByQuery
  , findModule
    -- * Searching for Classes
  , ClassQuery(..)
  , findClassesByQuery
  , findClass
  , isClass
    -- * Supermonad class detection
  , supermonadModuleName
  , bindClassName, returnClassName, applicativeClassName
  , findSupermonadModule
  , isBindClass, isReturnClass, isApplicativeClass
  , isSupermonadModule
  , findBindClass, findReturnClass, findApplicativeClass
  , findSupermonads
  , checkSupermonadInstances
    -- * Functor class detection
  , functorClassName, functorModuleName
    -- * General detection utilities
  , findInstancesInScope
  , findClassAndInstancesInScope
  ) where

import Data.List  ( find )
import Data.Either ( isLeft, isRight )
import Data.Maybe ( isNothing )
import qualified Data.Set as S

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
import Outputable ( SDoc, ($$), (<>), text, vcat, ppr, hang )
--import qualified Outputable as O

--import Control.Supermonad.Plugin.Log ( printObj, printObjTrace )
import Control.Supermonad.Plugin.Wrapper
  ( UnitId, moduleUnitId )
import Control.Supermonad.Plugin.Instance
  ( instanceTyArgs
  , isMonoTyConInstance 
  , isPolyTyConInstance )
import Control.Supermonad.Plugin.Utils
  ( collectTopTyCons, errIndent
  , fromRight, fromLeft )
import Control.Supermonad.Plugin.Dict
  ( SupermonadDict
  , BindInst, ApplicativeInst, ReturnInst
  , insertDict, emptyDict )
import Control.Supermonad.Plugin.Names

-- -----------------------------------------------------------------------------
-- Supermonad Class Detection
-- -----------------------------------------------------------------------------


findSupermonadModulesErrMsg :: [Either SDoc Module] -> SDoc
findSupermonadModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find supermonad or constrained supermonad modules!") errIndent (errA $$ errB)
findSupermonadModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained supermonad modules!"
findSupermonadModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls

-- | Checks if a module providing the supermonad classes is imported.
findSupermonadModule :: TcPluginM (Either SDoc Module)
findSupermonadModule = findModuleByQuery 
  $ EitherModule
    [ AnyModule [ ThisModule supermonadModuleName Nothing
                , ThisModule supermonadPreludeModuleName Nothing
                ]
    , AnyModule [ ThisModule supermonadCtModuleName Nothing
                , ThisModule supermonadCtPreludeModuleName Nothing
                ]
    ] $ Just findSupermonadModulesErrMsg

-- | Check if the given module is the supermonad module.
isSupermonadModule :: Module -> Bool
isSupermonadModule mdl = mdlName `elem` [smMdlName, smPrelName, smCtMdlName, smCtPrelName, mAIN_NAME]
  where mdlName = moduleName mdl
        smMdlName = mkModuleName supermonadModuleName
        smPrelName = mkModuleName supermonadPreludeModuleName
        smCtMdlName = mkModuleName supermonadCtModuleName
        smCtPrelName = mkModuleName supermonadCtPreludeModuleName

supermonadClassQuery :: Module -> ClassQuery
supermonadClassQuery smMdl = ClassQuery smMdl 
  [ (bindClassName       , 3)
  , (returnClassName     , 1)
  , (applicativeClassName, 3)
  ]

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Bind'
--   type class and is defined in the right module.
isBindClass :: Class -> Bool
isBindClass = isClass isSupermonadModule bindClassName 3

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Return'
--   type class and is defined in the right module.
isReturnClass :: Class -> Bool
isReturnClass = isClass isSupermonadModule returnClassName 1

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Applicative'
--   type class and is defined in the right module.
isApplicativeClass :: Class -> Bool
isApplicativeClass = isClass isSupermonadModule applicativeClassName 3

-- | Checks if a type class matching the shape and name of the
--   'Control.Supermonad.Bind' type class is in scope.
findBindClass :: TcPluginM (Maybe Class)
findBindClass = findClass isBindClass

-- | Checks if a type class matching the shape and name of the
--   'Control.Supermonad.Return' type class is in scope.
findReturnClass :: TcPluginM (Maybe Class)
findReturnClass = findClass isReturnClass

-- | Checks if a type class matching the shape and name of the 
--   'Control.Supermonad.Applicative' type class is in scope.
findApplicativeClass :: TcPluginM (Maybe Class)
findApplicativeClass = findClass isApplicativeClass

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

{- We don't actually need this.
-- | Defines the equality of module queries up to the error message construction
--   functions.
instance Eq ModuleQuery where
  (ThisModule mdlNameA unitIdA) == (ThisModule mdlNameB unitIdB) = mdlNameA == mdlNameB && unitIdA == unitIdB
  (EitherModule qas _) == (EitherModule qbs _) = qas == qbs
  (AnyModule qas) == (AnyModule qbs) = qas == qbs
  _ == _ = False
-}

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
    [] -> Left $ maybe defaultFindEitherModuleErrMsg id mErrFun $ eMdls
    [mdl] -> Right mdl
    _ -> Left $ maybe defaultFindEitherModuleErrMsg id mErrFun $ eMdls

-- | Makes sure that at least one of the given modules was found and, if so, returns 
--   the first one found (in order of the list). If none of the modules 
--   was found an error message will be returned.
--   The returned error message can be customized using the optional 
--   function.
findAnyModule :: Maybe ([SDoc] -> SDoc) -> [Either SDoc Module] -> (Either SDoc Module)
findAnyModule mErrFun eMdls = 
  case fmap fromRight $ filter isRight eMdls of
    [] -> Left $ maybe defaultFindAnyModuleErrMsg id mErrFun $ fmap fromLeft eMdls
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
data ClassQuery = ClassQuery Module [(PluginClassName, Arity)]

-- | Search for a collection of classes using the given query.
--   If any one of the classes could not be found an error is returned.
findClassesByQuery :: ClassQuery -> TcPluginM (Either SDoc [(PluginClassName, Class)])
findClassesByQuery (ClassQuery mdl toFindCls) = do
  eClss <- forM toFindCls $ \(clsName, clsArity) -> do
    eCls <- findClass (isClass (mdl ==) clsName clsArity)
    return (clsName, eCls, clsArity)
  let notFound = filter (\(_, c, _) -> isNothing c) eClss
  let errMsg :: (PluginClassName, Maybe Class, Arity) -> SDoc
      errMsg (n, _, a) = text $ "Could not find class '" ++ n ++ "' with arity " ++ show a ++ "!"
  return $ case notFound of
    [] -> Right $ fmap (\(n, Just c, _) -> (n, c)) $ eClss
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
-- Local Utility Functions
-- -----------------------------------------------------------------------------

-- | Tries to find a given class and all of its instances in scope 
--   using the class predicate.
findClassAndInstancesInScope :: (Class -> Bool) -> TcPluginM (Maybe (Class, [ClsInst]))
findClassAndInstancesInScope clsPred = do
  mCls <- findClass clsPred
  case mCls of
    Nothing -> return $ Nothing
    Just cls -> do
      insts <- findInstancesInScope cls
      return $ Just (cls, insts)

-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls

-- | Check if there are any supermonad instances that clearly 
--   do not belong to a specific supermonad.
checkSupermonadInstances 
  :: (Class, [BindInst]) -- ^ The @Bind@ class and instances.
  -> (Class, [ApplicativeInst]) -- ^ The @Applicative@ class and instances.
  -> (Class, [ReturnInst]) -- ^ The @Return@ class and instances.
  -> TcPluginM [(ClsInst, SDoc)]
checkSupermonadInstances (bindCls, bindInsts) (applicativeCls, applicativeInsts) (returnCls, returnInsts) = do
  let polyBindInsts        = filter (isPolyTyConInstance bindCls       ) bindInsts
  let polyApplicativeInsts = filter (isPolyTyConInstance applicativeCls) applicativeInsts
  let polyReturnInsts      = filter (isPolyTyConInstance returnCls     ) returnInsts
  
  return $ mconcat $
    [ fmap (\inst -> (inst, text "Not a valid supermonad instance: " $$ ppr inst)) polyBindInsts
    , fmap (\inst -> (inst, text "Not a valid supermonad instance: " $$ ppr inst)) polyApplicativeInsts
    , fmap (\inst -> (inst, text "Not a valid supermonad instance: " $$ ppr inst)) polyReturnInsts
    ]

-- | Constructs the map between type constructors and their supermonad instances.
findSupermonads 
  :: (Class, [BindInst]) -- ^ The @Bind@ class and instances.
  -> (Class, [ApplicativeInst]) -- ^ The @Applicative@ class and instances.
  -> (Class, [ReturnInst]) -- ^ The @Return@ class and instances.
  -> TcPluginM (SupermonadDict, [(TyCon, SDoc)])
  -- ^ Association between type constructors and their supermonad instances.
findSupermonads (bindCls, bindInsts) (applicativeCls, applicativeInsts) (returnCls, returnInsts) = do
  -- Collect all type constructors that are used for supermonads
  let supermonadTyCons = S.unions 
                       $ fmap (S.unions . fmap instTopTyCons) 
                       $ [bindInsts, applicativeInsts, returnInsts]
  -- Find the supermonad instances of each type constructor
  return $ mconcat 
         $ fmap findSupermonad
         $ S.toList supermonadTyCons
  where
    findSupermonad :: TyCon -> (SupermonadDict, [(TyCon, SDoc)])
    findSupermonad tc = 
      case ( filter (isMonoTyConInstance tc bindCls) bindInsts
           , filter (isMonoTyConInstance tc applicativeCls) applicativeInsts
           , filter (isMonoTyConInstance tc returnCls) returnInsts ) of
        ([bindInst], [applicativeInst], [returnInst]) -> 
          (insertDict tc (Just bindInst) applicativeInst returnInst emptyDict, [])
        ([], [applicativeInst], [returnInst]) -> 
          (insertDict tc Nothing applicativeInst returnInst emptyDict, [])
        (_, [], _) -> findError tc 
          $ text "Missing 'Applicative' instance for supermonad '" <> ppr tc <> text "'."
        (_, _, []) -> findError tc 
          $ text "Missing 'Return' instance for supermonad '" <> ppr tc <> text "'."
        (bindInsts', applicativeInsts', returnInsts') -> findError tc 
          $ text "Multiple 'Bind' instances for supermonad '" <> ppr tc <> text "':" $$ vcat (fmap ppr bindInsts')
          $$ text "Multiple 'Applicative' instances for supermonad '" <> ppr tc <> text "':" $$ vcat (fmap ppr applicativeInsts')
          $$ text "Multiple 'Return' instances for supermonad '" <> ppr tc <> text "':" $$ vcat (fmap ppr returnInsts')
    
    findError :: TyCon -> SDoc -> (SupermonadDict, [(TyCon, SDoc)])
    findError tc msg = (emptyDict, [(tc, msg)])
    
    -- | Collect the top-level type constructors in the arguments 
    --   of the given instance.
    instTopTyCons :: ClsInst -> S.Set TyCon
    instTopTyCons = collectTopTyCons . instanceTyArgs









