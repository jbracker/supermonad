
-- | Functions and utilities to detect the importent modules, classes
--   and types of the plugin.
module Control.Supermonad.Plugin.Detect
  ( -- * Supermonad class detection
    supermonadModuleName
  , bindClassName, returnClassName, applicativeClassName
  , findSupermonadModule
  , isBindClass, isReturnClass
  , isSupermonadModule
  , findBindClass, findReturnClass
  , findSupermonads
  , checkSupermonadInstances
    -- * Functor class detection
  , functorClassName, functorModuleName
    -- * General detection utilities
  , findInstancesInScope
  ) where

import Data.List  ( find )
import Data.Maybe ( catMaybes, listToMaybe )
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad ( forM, liftM )

import BasicTypes ( Arity )
import TcRnTypes
  ( TcGblEnv(..)
  , TcTyThing(..)
  , ImportAvails( imp_mods ) )
import Type ( TyThing(..) )
import TyCon ( TyCon )
import TcPluginM
  ( TcPluginM
  , getEnvs, getInstEnvs
  , tcLookup )
import Name
  ( nameModule
  , getOccName )
import OccName
  ( OccName
  , occNameString, mkTcOcc )
import RdrName
  ( GlobalRdrElt(..)
  , Parent( NoParent )
  , lookupGlobalRdrEnv )
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
import Outputable ( SDoc, ($$), (<>), text, vcat, ppr )
--import qualified Outputable as O

--import Control.Supermonad.Plugin.Log ( printObj, printObjTrace )
import Control.Supermonad.Plugin.Wrapper
  ( UnitId, baseUnitId, moduleUnitId, isImportedFrom )
import Control.Supermonad.Plugin.Instance
  ( instanceTyArgs
  , isMonoTyConInstance 
  , isPolyTyConInstance )
import Control.Supermonad.Plugin.Utils
  ( collectTopTyCons )

-- -----------------------------------------------------------------------------
-- Constant Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Supermonad" module.
supermonadModuleName :: String
supermonadModuleName = "Control.Supermonad"

-- | Name of the "Control.Supermonad.Constrained" module.
supermonadCtModuleName :: String
supermonadCtModuleName = "Control.Supermonad.Constrained"

-- | Name of the 'Control.Supermonad.Bind' type class.
bindClassName :: String
bindClassName = "Bind"

-- | Name of the 'Control.Supermonad.Bind' type class.
returnClassName :: String
returnClassName = "Return"

-- | Name of the 'Data.Functor.Functor' class.
functorClassName :: String
functorClassName = "Functor"

-- | Name of the 'Control.Supermonad.Applicative' type class.
applicativeClassName :: String
applicativeClassName = "Applicative"

-- | Name of the "Control.Supermonad.Prelude" module.
supermonadPreludeModuleName :: String
supermonadPreludeModuleName = "Control.Supermonad.Prelude"

-- | Name of the "Control.Supermonad.Constrained.Prelude" module.
supermonadCtPreludeModuleName :: String
supermonadCtPreludeModuleName = "Control.Supermonad.Constrained.Prelude"

-- | Name of the "Data.Functor" module.
functorModuleName :: String
functorModuleName = "Data.Functor"

-- -----------------------------------------------------------------------------
-- Polymonad Class Detection
-- -----------------------------------------------------------------------------

-- | Checks if a module providing the supermonad classes is imported.
findSupermonadModule :: TcPluginM (Either SDoc Module)
findSupermonadModule = do
  eSmUnCtMdl <- findSupermonadUnCtModule
  eSmCtMdl   <- findSupermonadCtModule
  case (eSmUnCtMdl, eSmCtMdl) of
    (Right _  , Left _errCt) -> return eSmUnCtMdl
    (Left _err, Right _    ) -> return eSmCtMdl
    (Left err, Left errCt) -> return $ Left
      $ text "Could not find supermonad or constrained supermonad modules!" $$ err $$ errCt
    (Right _, Right _) -> return $ Left
      $ text "Found unconstrained and constrained supermonad modules!"

-- | Checks if the module "Control.Supermonad" or "Control.Supermonad.Prelude"
--   is imported and, if so, returns either.
findSupermonadUnCtModule :: TcPluginM (Either SDoc Module)
findSupermonadUnCtModule = do
  eMdl <- getModule Nothing supermonadModuleName
  case eMdl of
    Left _err -> getModule Nothing supermonadPreludeModuleName
    Right _ -> return eMdl

-- | Checks if the module "Control.Supermonad.Constrained" or "Control.Supermonad.Constrained.Prelude"
--   is imported and, if so, returns either.
findSupermonadCtModule :: TcPluginM (Either SDoc Module)
findSupermonadCtModule = do
  eCtMdl <- getModule Nothing supermonadCtModuleName
  case eCtMdl of
    Left _err -> getModule Nothing supermonadCtPreludeModuleName
    Right _ -> return eCtMdl

-- | Check if the given module is the supermonad module.
isSupermonadModule :: Module -> Bool
isSupermonadModule mdl = mdlName `elem` [smMdlName, smPrelName, smCtMdlName, smCtPrelName, mAIN_NAME]
  where mdlName = moduleName mdl
        smMdlName = mkModuleName supermonadModuleName
        smPrelName = mkModuleName supermonadPreludeModuleName
        smCtMdlName = mkModuleName supermonadCtModuleName
        smCtPrelName = mkModuleName supermonadCtPreludeModuleName

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
-- Local Utility Functions
-- -----------------------------------------------------------------------------

-- | Tries to find all of the given modules using the given search functions.
--   Returns the list of all found modules.
findModules :: [TcPluginM (Either SDoc Module)] -> TcPluginM [Module]
findModules findMdls = do
  eitherMdls <- sequence findMdls
  return $ catMaybes $ fmap (either (const Nothing) Just) eitherMdls

-- | Checks if the module with the given name is imported and,
--   if so, returns that module.
getModule :: Maybe UnitId -> String -> TcPluginM (Either SDoc Module)
getModule pkgKeyToFind mdlNameToFind = do
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
  
-- For some reason this version also found modules that are not in the
-- imports.
{-
-- | Checks if the module with the given name is imported and,
--   if so, returns that module.
getModule :: Maybe UnitId -> String -> TcPluginM (Either SDoc Module)
getModule pkgKeyToFind mdlNameToFind = do
  mdlResult <- findImportedModule (mkModuleName mdlNameToFind) Nothing -- From "TcPluginM"
  case mdlResult of
    Found _mdlLoc mdl ->
      if maybe True (moduleUnitId mdl ==) pkgKeyToFind then
        return $ Right mdl
      else
        return $ Left
          $  text "Package key of found module does not match the requested key:"
          $$ text "Found:     " <> ppr (moduleUnitId mdl)
          $$ text "Requested: " <> ppr pkgKeyToFind
    NoPackage pkgKey -> return $ Left
      $ text "Found module, but missing its package: " <> ppr pkgKey
    FoundMultiple mdls -> return $ Left
      $  text ("Module '" ++ mdlNameToFind ++ "' appears in several packages:")
      $$ ppr (fmap snd mdls)
    NotFound {} -> return $ Left 
      $ text $ "Module was not found: " ++ mdlNameToFind
-}

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

-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls

-- | Check if there are any supermonad instances that clearly 
--   do not belong to a specific supermonad.
checkSupermonadInstances 
  :: Class -- ^ 'Control.Supermonad.Bind' type class.
  -> Class -- ^ 'Control.Supermonad.Return' type class.
  -> TcPluginM [(ClsInst, SDoc)]
checkSupermonadInstances bindCls returnCls = do
    bindInsts   <- findInstancesInScope bindCls
    returnInsts <- findInstancesInScope returnCls
    
    let polyBindInsts   = filter (isPolyTyConInstance bindCls  ) bindInsts
    let polyReturnInsts = filter (isPolyTyConInstance returnCls) returnInsts
    
    return $  fmap (\inst -> (inst, text "Not a valid supermonad instance: " $$ ppr inst)) polyBindInsts 
           ++ fmap (\inst -> (inst, text "Not a valid supermonad instance: " $$ ppr inst)) polyReturnInsts

-- | Constructs the map between type constructors and their supermonad instance.
findSupermonads 
  :: Class -- ^ 'Control.Supermonad.Bind' type class.
  -> Class -- ^ 'Control.Supermonad.Return' type class.
  -> TcPluginM (M.Map TyCon (ClsInst, ClsInst), [(TyCon, SDoc)])
  -- ^ Association between type constructor and its 
  --   'Control.Supermonad.Bind' and 'Control.Supermonad.Return' 
  --   instance in that order.
findSupermonads bindCls returnCls = do
  bindInsts   <- findInstancesInScope bindCls
  returnInsts <- findInstancesInScope returnCls
  -- Collect all type constructors that are used for supermonads
  let supermonadTyCons = S.unions $ fmap instTopTyCons $ bindInsts ++ returnInsts
  -- Find the supermonad instances of each type constructor
  return $ mconcat 
          $ fmap (findSupermonad bindInsts returnInsts)
          $ S.toList supermonadTyCons
  where
    findSupermonad :: [ClsInst] -> [ClsInst] -> TyCon -> (M.Map TyCon (ClsInst, ClsInst), [(TyCon, SDoc)])
    findSupermonad bindInsts returnInsts tc = 
      case ( filter (isMonoTyConInstance tc bindCls) bindInsts
           , filter (isMonoTyConInstance tc returnCls) returnInsts ) of
        ([bindInst], [returnInst]) -> (M.singleton tc (bindInst, returnInst), [])
        ([], _) -> findError tc 
          $ text "Missing 'Bind' instance for supermonad '" <> ppr tc <> text "'."
        (_, []) -> findError tc 
          $ text "Missing 'Return' instance for supermonad '" <> ppr tc <> text "'."
        (bindInsts', returnInsts') -> findError tc 
          $ text "Multiple 'Bind' instances for supermonad '" <> ppr tc <> text "':" $$ vcat (fmap ppr bindInsts')
          $$ text "Multiple 'Return' instances for supermonad '" <> ppr tc <> text "':" $$ vcat (fmap ppr returnInsts')
    
    findError :: TyCon -> SDoc -> (M.Map TyCon (ClsInst, ClsInst), [(TyCon, SDoc)])
    findError tc msg = (M.empty, [(tc, msg)])
    
    -- | Collect the top-level type constructors in the arguments 
    --   of the given instance.
    instTopTyCons :: ClsInst -> S.Set TyCon
    instTopTyCons = collectTopTyCons . instanceTyArgs









