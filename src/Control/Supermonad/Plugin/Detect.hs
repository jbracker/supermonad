
-- | Functions and utilities to detect the importent modules, classes
--   and types of the plugin.
module Control.Supermonad.Plugin.Detect
  ( -- * Supermonad class detection
    supermonadModuleName
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
  ) where

import Data.List  ( find )
--import Data.Maybe ( catMaybes )
import qualified Data.Set as S

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
import Outputable ( SDoc, ($$), (<>), text, vcat, ppr )
--import qualified Outputable as O

--import Control.Supermonad.Plugin.Log ( printObj, printObjTrace )
import Control.Supermonad.Plugin.Wrapper
  ( UnitId, moduleUnitId )
import Control.Supermonad.Plugin.Instance
  ( instanceTyArgs
  , isMonoTyConInstance 
  , isPolyTyConInstance )
import Control.Supermonad.Plugin.Utils
  ( collectTopTyCons )
import Control.Supermonad.Plugin.Dict
  ( SupermonadDict
  , BindInst, ApplicativeInst, ReturnInst
  , insertDict, emptyDict )
import Control.Supermonad.Plugin.Names

-- -----------------------------------------------------------------------------
-- Polymonad Class Detection
-- -----------------------------------------------------------------------------

-- | Checks if a module providing the supermonad classes is imported.
findSupermonadModule :: TcPluginM (Either SDoc Module)
findSupermonadModule = do
  -- Check if the module "Control.Supermonad" or "Control.Supermonad.Prelude" is imported.
  eSmUnCtMdl <- findAnyModule [(Nothing, supermonadModuleName  ), (Nothing, supermonadPreludeModuleName  )]
  -- Checks if the module "Control.Supermonad.Constrained" or "Control.Supermonad.Constrained.Prelude" is imported.
  eSmCtMdl   <- findAnyModule [(Nothing, supermonadCtModuleName), (Nothing, supermonadCtPreludeModuleName)]
  case (eSmUnCtMdl, eSmCtMdl) of
    (Right _  , Left _errCt) -> return eSmUnCtMdl
    (Left _err, Right _    ) -> return eSmCtMdl
    (Left err, Left errCt) -> return $ Left
      $ text "Could not find supermonad or constrained supermonad modules!" $$ err $$ errCt
    (Right _, Right _) -> return $ Left
      $ text "Found unconstrained and constrained supermonad modules!"


-- | Checks if any of the given modules is imported and, if so, returns 
--   the first one it finds in order of the list. If none of the modules 
--   exists an error message will be returned.
findAnyModule :: [(Maybe UnitId, String)] -> TcPluginM (Either SDoc Module)
findAnyModule = findAnyModule' []
  where 
    findAnyModule' :: [SDoc] -> [(Maybe UnitId, String)] -> TcPluginM (Either SDoc Module)
    findAnyModule' errs [] = do
      -- Need to reverse, because accumulation stacks them that way.
      return $ Left $ vcat $ reverse errs
    findAnyModule' errs ((mdlUnit, mdlName) : mdls) = do
      eMdl <- getModule mdlUnit mdlName
      case eMdl of
        Left err -> findAnyModule' (err : errs) mdls
        Right _ -> return eMdl

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

-- | Checks if the given class matches the shape of the 'Control.Supermonad.Applicative'
--   type class and is defined in the right module.
isApplicativeClass :: Class -> Bool
isApplicativeClass cls = isClass cls isSupermonadModule applicativeClassName 3

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
-- Local Utility Functions
-- -----------------------------------------------------------------------------
{-
-- | Tries to find all of the given modules using the given search functions.
--   Returns the list of all found modules.
findModules :: [TcPluginM (Either SDoc Module)] -> TcPluginM [Module]
findModules findMdls = do
  eitherMdls <- sequence findMdls
  return $ catMaybes $ fmap (either (const Nothing) Just) eitherMdls
-}
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
{-
-- | Check if the given element has no parents.
hasNoParent :: GlobalRdrElt -> Bool
hasNoParent rdrElt = case gre_par rdrElt of
  NoParent -> True
  _ -> False
-}
-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls

-- | Check if there are any supermonad instances that clearly 
--   do not belong to a specific supermonad.
checkSupermonadInstances 
  :: Class -- ^ 'Control.Supermonad.Bind' type class.
  -> Class -- ^ 'Control.Supermonad.Applicative' type class.
  -> Class -- ^ 'Control.Supermonad.Return' type class.
  -> TcPluginM [(ClsInst, SDoc)]
checkSupermonadInstances bindCls applicativeCls returnCls = do
  bindInsts        <- findInstancesInScope bindCls
  applicativeInsts <- findInstancesInScope applicativeCls
  returnInsts      <- findInstancesInScope returnCls
  
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
  :: Class -- ^ 'Control.Supermonad.Bind' type class.
  -> Class -- ^ 'Control.Supermonad.Applicative' type class.
  -> Class -- ^ 'Control.Supermonad.Return' type class.
  -> TcPluginM (SupermonadDict, [(TyCon, SDoc)])
  -- ^ Association between type constructors and their supermonad instances.
findSupermonads bindCls applicativeCls returnCls = do
  bindInsts        <- findInstancesInScope bindCls
  applicativeInsts <- findInstancesInScope applicativeCls
  returnInsts      <- findInstancesInScope returnCls
  -- Collect all type constructors that are used for supermonads
  let supermonadTyCons = S.unions 
                       $ fmap (S.unions . fmap instTopTyCons) 
                       $ [bindInsts, applicativeInsts, returnInsts]
  -- Find the supermonad instances of each type constructor
  return $ mconcat 
         $ fmap (findSupermonad bindInsts applicativeInsts returnInsts) 
         $ S.toList supermonadTyCons
  where
    findSupermonad :: [BindInst] -> [ApplicativeInst] -> [ReturnInst] -> TyCon 
                   -> (SupermonadDict, [(TyCon, SDoc)])
    findSupermonad bindInsts applicativeInsts returnInsts tc = 
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









