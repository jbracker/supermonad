
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
  --, areBindFunctorArguments
  --, areBindApplyArguments
  --, findFunctorBindInstances
  , functorClassName, functorModuleName
    -- * General detection utilities
  , findInstancesInScope
  ) where

import Data.Maybe ( catMaybes, listToMaybe )
import Control.Monad ( forM, liftM )

import BasicTypes ( Arity )
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
  , pprToStr ) --, printObj, printObjTrace )
import Control.Supermonad.Plugin.Instance
  ( instanceType )
import Control.Supermonad.Plugin.Utils
  ( getTyConName )

-- -----------------------------------------------------------------------------
-- Constant Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Supermonad" module.
supermonadModuleName :: String
supermonadModuleName = "Control.Supermonad"

-- | Name of the 'Control.Supermonad.Bind' type class.
bindClassName :: String
bindClassName = "Bind"

-- | Name of the 'Control.Supermonad.Bind' type class.
returnClassName :: String
returnClassName = "Return"

-- | Name of the "Data.Functor.Identity" module.
identityModuleName :: String
identityModuleName = "Data.Functor.Identity"

-- | Name of the 'Data.Functor.Identity.Identity' type constructor.
identityTyConName :: String
identityTyConName = "Identity"

-- | Name of the "Control.Supermonad.Prelude" module.
supermonadPreudeModuleName :: String
supermonadPreudeModuleName = "Control.Supermonad.Prelude"

-- | Name of the 'Data.Functor.Functor' class.
functorClassName :: String
functorClassName = "Functor"

-- | Name of the "Data.Functor" module.
functorModuleName :: String
functorModuleName = "Data.Functor"

-- -----------------------------------------------------------------------------
-- Polymonad Class Detection
-- -----------------------------------------------------------------------------

-- | Checks if the module "Control.Supermonad"
--   is imported and, if so, returns the module.
findSupermonadModule :: TcPluginM (Either String Module)
findSupermonadModule = do
  eitherMdl <- getModule Nothing supermonadModuleName
  case eitherMdl of
    Left _err -> findSupermonadPreludeModule
    Right mdl -> return $ Right mdl

-- | Checks if the module "Control.Supermonad.Prelude"
--   is imported and, if so, returns the module.
findSupermonadPreludeModule :: TcPluginM (Either String Module)
findSupermonadPreludeModule = getModule Nothing supermonadPreudeModuleName

-- | Check if the given module is the supermonad module.
isSupermonadModule :: Module -> Bool
isSupermonadModule mdl = mdlName `elem` [pmMdlName, pmPrelName, mAIN_NAME]
  where mdlName = moduleName mdl
        pmMdlName = mkModuleName supermonadModuleName
        pmPrelName = mkModuleName supermonadPreudeModuleName

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Bind'
--   type class and is defined in the right module.
isBindClass :: Class -> Bool
isBindClass cls = isClass cls isSupermonadModule bindClassName 3

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Return'
--   type class and is defined in the right module.
isReturnClass :: Class -> Bool
isReturnClass cls = isClass cls isSupermonadModule returnClassName 1

-- | Checks if a type class matching the shape and name of the
--   'Control.Supermonad.Bind' type class is in scope.
findBindClass :: TcPluginM (Maybe Class)
findBindClass = findClass isBindClass

-- | Checks if a type class matching the shape and name of the
--   'Control.Supermonad.Return' type class is in scope.
findReturnClass :: TcPluginM (Maybe Class)
findReturnClass = findClass isReturnClass

-- -----------------------------------------------------------------------------
-- Identity Type Detection
-- -----------------------------------------------------------------------------

-- | Checks if the module "Data.Functor.Identity"
--   is imported and, if so, returns the module.
findIdentityModule :: TcPluginM (Either String Module)
findIdentityModule = getModule (Just basePackageKey) identityModuleName

-- | Tries to find the 'Data.Functor.Identity.Identity' type constructor in the imported
--   modules. Only looks for imports through specific modules.
findIdentityTyCon :: TcPluginM (Maybe TyCon)
findIdentityTyCon = do
  mdls <- findModules [findIdentityModule, findSupermonadModule, findSupermonadPreludeModule]
  case mdls of
    [] -> return Nothing
    _ -> findTyConByNameAndModule (mkTcOcc identityTyConName) mdls

-- -----------------------------------------------------------------------------
-- Functor Bind Instance Detection
-- -----------------------------------------------------------------------------
{-
-- | Check if the given type arguments would form a 'Control.Supermonad.Bind' functor instance 
--   if applied to 'Control.Supermonad.Bind': @Bind m Identity m@
areBindFunctorArguments :: TyCon -- ^ The 'Data.Functor.Identity.Identity' type constructor.
                        -> Type -> Type -> Type -> Bool
areBindFunctorArguments idTyCon t1 t2 t3 =
  let idTC = mkTyConTy idTyCon
  in eqType t2 idTC && eqType t1 t3 -- Bind m Identity m

-- | Check if the given type arguments would form a 'Control.Supermonad.Bind' apply instance 
--   if applied to 'Control.Supermonad.Bind': @Bind Identity m m@
areBindApplyArguments :: TyCon -- ^ The 'Data.Functor.Identity.Identity' type constructor.
                      -> Type -> Type -> Type -> Bool
areBindApplyArguments idTyCon t1 t2 t3 =
  let idTC = mkTyConTy idTyCon
  in eqType t1 idTC && eqType t2 t3 -- Bind Identity m m

-- | Check if the given instance is a 'Control.Supermonad.Bind' functor instance: @Bind m Identity m@
isFunctorBindInstance :: Class -- ^ 'Control.Supermonad.Bind' class.
                      -> TyCon -- ^ 'Data.Functor.Identity.Identity' type constructor.
                      -> ClsInst -> Bool
isFunctorBindInstance bindCls idTyCon inst = 
  let (_cts, cls, _tc, args) = instanceType inst
  in cls == bindCls && hasOnlyFunctorConstraint inst && case args of
    [t1, t2, t3] -> areBindFunctorArguments idTyCon t1 t2 t3 -- Bind m Identity m
    _ -> False

-- | Check if the given instance is a 'Control.Supermonad.Bind' apply instance: @Bind Identity m m@
isApplyBindInstance :: Class -- ^ 'Control.Supermonad.Bind' class.
                    -> TyCon -- ^ 'Identity' type constructor.
                    -> ClsInst -> Bool
isApplyBindInstance bindCls idTyCon inst =
  let (_cts, cls, _tc, args) = instanceType inst
  in cls == bindCls && hasOnlyFunctorConstraint inst && case args of
    [t1, t2, t3] -> areBindApplyArguments idTyCon t1 t2 t3 -- Bind Identity m m
    _ -> False

-- | Check if the given type constructor is the 'Data.Functor.Functor' type constructor.
isFunctorTyCon :: TyCon -> Bool
isFunctorTyCon tc = isClassTyCon tc && getTyConName tc == functorClassName

-- | Check if the given type is an application of the 'Data.Functor.Functor' 
--   type constructor to some type arguments.
isFunctorTyConApp :: Type -> Bool
isFunctorTyConApp t = case splitTyConApp_maybe t of
  Just (ctTyCon, ctArgs) | length ctArgs == 1 -> isFunctorTyCon ctTyCon
  _ -> False

-- | Check if the constraints of the given instance only contain 
--   'Data.Functor.Functor' class constraints.
hasOnlyFunctorConstraint :: ClsInst -> Bool
hasOnlyFunctorConstraint inst =
  let (cts, _, _, _) = instanceType inst
  in (length cts <= 2) && all isFunctorTyConApp cts

-- | Returns the pair of 'Control.Supermonad.Bind' 
--   instances @(Bind m Identity m, Bind Identity n n)@ if these instances can be found.
findFunctorBindInstances :: Class -- ^ 'Control.Supermonad.Bind' class.
                         -> TyCon -- ^ 'Data.Functor.Identity.Identity' type constructor.
                         -> TcPluginM (Maybe (ClsInst, ClsInst))
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
-}
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

-- | Check if the given class has the given name, arity and if the classes
--   module fulfills the given predicate.
isClass :: Class -> (Module -> Bool) -> String -> Arity -> Bool
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

-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls
