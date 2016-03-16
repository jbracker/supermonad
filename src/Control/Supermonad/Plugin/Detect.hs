
-- | Functions and utilities to detect the importent modules, classes
--   and types of the plugin.
module Control.Supermonad.Plugin.Detect
  ( -- * Supermonad Class Detection
    supermonadModuleName
  , bindClassName, returnClassName
  , findSupermonadModule
  , findSupermonadPreludeModule
  , isBindClass, isReturnClass
  , isSupermonadModule
  , findBindClass, findReturnClass
    -- * Identity Type Detection
  , identityModuleName
  , identityTyConName
  , findIdentityModule
  , findIdentityTyCon
    -- * Functor Bind Instance Detection
  , areBindFunctorArguments
  , areBindApplyArguments
  , findFunctorBindInstances
  , functorClassName, functorModuleName
    -- * General detection utilities
  , findInstancesInScope
  ) where

import Data.Maybe ( catMaybes, listToMaybe )
import Control.Monad ( forM, liftM )

import TcRnTypes
  ( TcGblEnv(..)
  , TcTyThing(..) )
import Type
  ( Type, TyThing(..)
  , mkTyConTy
  , splitTyConApp_maybe
  , eqType )
import TyCon ( TyCon, isClassTyCon )
import TcPluginM
  ( TcPluginM
  , getEnvs, getInstEnvs
  , findImportedModule, FindResult(..)
  , tcLookup )
import Name
  ( nameModule
  , getOccName )
import OccName
  ( OccName
  , occNameString, mkTcOcc )
import RdrName
  ( GlobalRdrElt(..)
  , Parent( NoParent ), Provenance(..)
  , importSpecModule
  , lookupGlobalRdrEnv )
import Module
  ( Module(..), PackageKey
  , basePackageKey
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

import Control.Supermonad.Plugin.Log
  ( pmErrMsg
  , pprToStr ) -- , printObj, printObjTrace )
import Control.Supermonad.Plugin.Instance
  ( instanceType )
import Control.Supermonad.Plugin.Utils
  ( getTyConName )

-- -----------------------------------------------------------------------------
-- Constant Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the 'Control.Supermonad' module.
supermonadModuleName :: String
supermonadModuleName = "Control.Supermonad"

-- | Name of the 'Bind' type class.
bindClassName :: String
bindClassName = "Bind"

-- | Name of the 'Bind' type class.
returnClassName :: String
returnClassName = "Return"

-- | Name of the 'Data.Functor.Identity' module.
identityModuleName :: String
identityModuleName = "Data.Functor.Identity"

-- | Name of the 'Identity' type constructor.
identityTyConName :: String
identityTyConName = "Identity"

supermonadPreudeModuleName :: String
supermonadPreudeModuleName = "Control.Supermonad.Prelude"

functorClassName :: String
functorClassName = "Functor"

functorModuleName :: String
functorModuleName = "Data.Functor"

-- -----------------------------------------------------------------------------
-- Polymonad Class Detection
-- -----------------------------------------------------------------------------

-- | Checks if the module 'Control.Supermonad'
--   is imported and, if so, returns the module.
findSupermonadModule :: TcPluginM (Either String Module)
findSupermonadModule = do
  eitherMdl <- getModule Nothing supermonadModuleName
  case eitherMdl of
    Left _err -> findSupermonadPreludeModule
    Right mdl -> return $ Right mdl

-- | Checks if the module 'Control.Supermonad.Prelude'
--   is imported and, if so, returns the module.
findSupermonadPreludeModule :: TcPluginM (Either String Module)
findSupermonadPreludeModule = getModule Nothing supermonadPreudeModuleName

-- | Check if the given module is the supermonad module.
isSupermonadModule :: Module -> Bool
isSupermonadModule mdl = mdlName `elem` [pmMdlName, pmPrelName, mAIN_NAME]
  where mdlName = moduleName mdl
        pmMdlName = mkModuleName supermonadModuleName
        pmPrelName = mkModuleName supermonadPreudeModuleName

-- | Checks if the given class matches the shape of the 'Bind'
--   type class and is defined in the right module.
isBindClass :: Class -> Bool
isBindClass cls = isClass cls isSupermonadModule bindClassName 2

-- | Checks if the given class matches the shape of the 'Return'
--   type class and is defined in the right module.
isReturnClass :: Class -> Bool
isReturnClass cls = isClass cls isSupermonadModule returnClassName 1

-- | Checks if a type class matching the shape and name of the
--   'Bind' type class is in scope.
findBindClass :: TcPluginM (Maybe Class)
findBindClass = findClass isBindClass

-- | Checks if a type class matching the shape and name of the
--   'Bind' type class is in scope.
findReturnClass :: TcPluginM (Maybe Class)
findReturnClass = findClass isReturnClass

-- -----------------------------------------------------------------------------
-- Identity Type Detection
-- -----------------------------------------------------------------------------

-- | Checks if the module 'Data.Functor.Identity'
--   is imported and, if so, returns the module.
findIdentityModule :: TcPluginM (Either String Module)
findIdentityModule = getModule (Just basePackageKey) identityModuleName

-- | Tries to find the identity type constructor in the imported
--   modules. Will accept the constructor if it is imported through
--   either 'Data.Functor.Identity' or 'Control.Polymonad'.
findIdentityTyCon :: TcPluginM (Maybe TyCon)
findIdentityTyCon = do
  mdls <- findModules [findIdentityModule, findSupermonadModule, findSupermonadPreludeModule]
  case mdls of
    [] -> return Nothing
    _ -> findTyConByNameAndModule (mkTcOcc identityTyConName) mdls

-- -----------------------------------------------------------------------------
-- Functor Bind Instance Detection
-- -----------------------------------------------------------------------------

areBindFunctorArguments :: TyCon -> Type -> Type -> Bool
areBindFunctorArguments idTyCon _t1 t2 =
  let idTC = mkTyConTy idTyCon
  in eqType t2 idTC -- && eqType t1 t3 -- Bind m Identity

areBindApplyArguments :: TyCon -> Type -> Type -> Bool
areBindApplyArguments idTyCon t1 _t2 =
  let idTC = mkTyConTy idTyCon
  in eqType t1 idTC -- && eqType t2 t3 -- Bind Identity m

isFunctorBindInstance :: Class -> TyCon -> ClsInst -> Bool
isFunctorBindInstance bindCls idTyCon inst = 
  let (_cts, cls, _tc, args) = instanceType inst
  in cls == bindCls && hasOnlyFunctorConstraint inst && case args of
    [t1, t2] -> areBindFunctorArguments idTyCon t1 t2 -- Bind m Identity
    _ -> False

isApplyBindInstance :: Class -> TyCon -> ClsInst -> Bool
isApplyBindInstance bindCls idTyCon inst =
  let (_cts, cls, _tc, args) = instanceType inst
  in cls == bindCls && hasOnlyFunctorConstraint inst && case args of
    [t1, t2] -> areBindApplyArguments idTyCon t1 t2 -- Bind Identity m
    _ -> False

isFunctorTyCon :: TyCon -> Bool
isFunctorTyCon tc = isClassTyCon tc && getTyConName tc == functorClassName

isFunctorTyConApp :: Type -> Bool
isFunctorTyConApp t = case splitTyConApp_maybe t of
  Just (ctTyCon, ctArgs) | length ctArgs == 1 -> isFunctorTyCon ctTyCon
  _ -> False

hasOnlyFunctorConstraint :: ClsInst -> Bool
hasOnlyFunctorConstraint inst =
  let (cts, _, _, _) = instanceType inst
  in (length cts <= 3) && all isFunctorTyConApp cts

-- | Requires the bind class and identity type constructor as argument.
--   Returns the pair of instances (Bind m Identity m, Bind Identity n n)
--   if these instances can be found.
findFunctorBindInstances :: Class -> TyCon -> TcPluginM (Maybe (ClsInst, ClsInst))
findFunctorBindInstances bindCls idTyCon = do
  let isFunctorBindInst = isFunctorBindInstance bindCls idTyCon
  let isApplyBindInst = isApplyBindInstance bindCls idTyCon
  bindInsts <-  filter (\inst -> isFunctorBindInst inst || isApplyBindInst inst) 
            <$> findInstancesInScope bindCls
  case bindInsts of
    [b1, b2] -> do
      if isFunctorBindInst b1 then
        return $ Just (b1, b2)
      else
        return $ Just (b2, b1)
    _ -> return Nothing

-- -----------------------------------------------------------------------------
-- Local Utility Functions
-- -----------------------------------------------------------------------------

-- | Tries to find all of the given modules using the given search functions.
--   Returns the list of all found modules.
findModules :: [TcPluginM (Either String Module)] -> TcPluginM [Module]
findModules findMdls = do
  eitherMdls <- sequence findMdls
  return $ catMaybes $ fmap (either (const Nothing) Just) eitherMdls

-- | Checks if the module with the given name is imported and,
--   if so, returns that module.
getModule :: Maybe PackageKey -> String -> TcPluginM (Either String Module)
getModule pkgKeyToFind mdlNameToFind = do
  mdlResult <- findImportedModule (mkModuleName mdlNameToFind) Nothing
  case mdlResult of
    Found _mdlLoc mdl ->
      if maybe True (modulePackageKey mdl ==) pkgKeyToFind then
        return $ Right mdl
      else
        return $ Left $ pmErrMsg
          $  "Package key of found module (" ++ pprToStr (modulePackageKey mdl) ++ ")"
          ++ " does not match the requested key (" ++ pprToStr pkgKeyToFind ++ ")."
    NoPackage pkgKey -> return $ Left $ pmErrMsg
      $ "Found module, but missing its package: " ++ pprToStr pkgKey
    FoundMultiple mdls -> return $ Left $ pmErrMsg
      $ "Module " ++ mdlNameToFind ++ " appears in several packages:\n"
      ++ pprToStr (fmap snd mdls)
    NotFound {} -> return $ Left $ pmErrMsg "Module was not found."

-- | Checks if a type class matching the shape of the given 
--   predicate is in scope.
findClass :: (Class -> Bool) -> TcPluginM (Maybe Class)
findClass isClass' = do
  let isCls = isClass' . is_cls
  envs <- fst <$> getEnvs
  -- This is needed while compiling the package itself...
  let foundInstsLcl =  (filter isCls . instEnvElts . tcg_inst_env $ envs)
                    ++ (filter isCls . tcg_insts $ envs)
  -- This is needed while compiling an external package depending on it...
  foundInstsGbl <- filter isCls . instEnvElts . ie_global <$> getInstEnvs
  return $ case foundInstsLcl ++ foundInstsGbl of
    (inst : _) -> Just $ is_cls inst
    [] -> Nothing

isClass :: Class -> (Module -> Bool) -> String -> Int -> Bool
isClass cls isModule targetClassName targetArity =
  let clsName = className cls
      clsMdl = nameModule clsName
      clsNameStr = occNameString $ getOccName clsName
      clsArity = classArity cls
  in    isModule clsMdl
     && clsNameStr == targetClassName
     && clsArity == targetArity

-- | Try to find a type constructor given its name and the modules it
--   is exported from. The type constructor needs to be imported from
--   one of these modules.
findTyConByNameAndModule :: OccName -> [Module] -> TcPluginM (Maybe TyCon)
findTyConByNameAndModule occName mdls = do
  -- Look at the global environment of names that are in scope.
  rdrEnv <- tcg_rdr_env . fst <$> getEnvs
  -- Search for things that have the same name as what we are looking for.
  let envResultElem = lookupGlobalRdrEnv rdrEnv occName
  -- Only keep things that are originally from our module and have no parents,
  -- because type constructors are declared on top-level.
  let relResults = filter
        (\e -> any (e `isImportedFrom`) mdls && hasNoParent e)
        envResultElem
  -- Find all the typed things that have the same name as the stuff we found.
  -- Also directly convert them into type constructors if possible
  mTyCons <- forM relResults $ liftM tcTyThingToTyCon . tcLookup . gre_name
  -- Only keep those things that actually were type constructors.
  let tyCons = catMaybes mTyCons
  -- In theory, we should not find more then one type constructor,
  -- because that would lead to a name clash in the source module
  -- and we made sure to only look at one module.
  return $ listToMaybe tyCons

-- | Try to convert the given typed thing into a type constructor.
tcTyThingToTyCon :: TcTyThing -> Maybe TyCon
tcTyThingToTyCon (AGlobal (ATyCon tc)) = Just tc
tcTyThingToTyCon _ = Nothing

-- | Check if the given element has no parents.
hasNoParent :: GlobalRdrElt -> Bool
hasNoParent rdrElt = case gre_par rdrElt of
  NoParent -> True
  _ -> False

-- | Check if the given element is imported from the given module.
isImportedFrom :: GlobalRdrElt -> Module -> Bool
isImportedFrom rdrElt mdl = case gre_prov rdrElt of
  LocalDef -> False
  Imported [] -> False
  Imported impSpecs -> moduleName mdl == importSpecModule (last impSpecs)

-- | Returns a list of all 'Control.Polymonad' instances that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls
