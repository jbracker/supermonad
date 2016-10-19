
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
  , findAnyModule, findEitherModule
  , isClass
  , findClass
  , findInstancesInScope
  , findClassAndInstancesInScope
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
  ( collectTopTyCons, errIndent )
import Control.Supermonad.Plugin.Dict
  ( SupermonadDict
  , BindInst, ApplicativeInst, ReturnInst
  , insertDict, emptyDict )
import Control.Supermonad.Plugin.Names

-- -----------------------------------------------------------------------------
-- Polymonad Class Detection
-- -----------------------------------------------------------------------------

-- | Makes sure that only one of the given modules was found and returns that
--   found module. If both or none of them were found an error is returned.
--   The returned error message can be customized using the optional 
--   function.
findEitherModule :: (Either SDoc Module) -> (Either SDoc Module) -> Maybe (Maybe (SDoc, SDoc) -> SDoc) -> (Either SDoc Module)
findEitherModule eMdlA eMdlB mErrFun = case (eMdlA, eMdlB) of
    (Right _mdlA, Left  _errB) -> eMdlA
    (Left  _errA, Right _mdlB) -> eMdlB
    (Left   errA, Left   errB) -> 
      let err = text "Failed to find either module!" 
              $$ hang (text "First module error:") errIndent errA 
              $$ hang (text "Second module error:") errIndent errB
      in Left $ maybe err ($ Just (errA, errB)) mErrFun
    (Right  mdlA, Right  mdlB) -> 
      let err = hang (text "Found both modules:") errIndent $ ppr mdlA $$ ppr mdlB
      in Left $ maybe err ($ Nothing) mErrFun

-- | Checks if a module providing the supermonad classes is imported.
findSupermonadModule :: TcPluginM (Either SDoc Module)
findSupermonadModule = do
  -- Check if the module "Control.Supermonad" or "Control.Supermonad.Prelude" is imported.
  eSmUnCtMdl <- findAnyModule [(Nothing, supermonadModuleName  ), (Nothing, supermonadPreludeModuleName  )]
  -- Checks if the module "Control.Supermonad.Constrained" or "Control.Supermonad.Constrained.Prelude" is imported.
  eSmCtMdl   <- findAnyModule [(Nothing, supermonadCtModuleName), (Nothing, supermonadCtPreludeModuleName)]
  
  return $ findEitherModule eSmUnCtMdl eSmCtMdl $ Just 
         $ \mErr -> maybe (text "Found unconstrained and constrained supermonad modules!")
                          (\(err, errCt) -> hang (text "Could not find supermonad or constrained supermonad modules!") errIndent (err $$ errCt))
                          mErr


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
-- Local Utility Functions
-- -----------------------------------------------------------------------------

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
isClass :: (Module -> Bool) -> String -> Arity -> Class -> Bool
isClass isModule targetClassName targetArity cls =
  let clsName = className cls
      clsMdl = nameModule clsName
      clsNameStr = occNameString $ getOccName clsName
      clsArity = classArity cls
  in    isModule clsMdl
     && clsNameStr == targetClassName
     && clsArity == targetArity

-- | Returns a list of all instances for the given class that are currently in scope.
findInstancesInScope :: Class -> TcPluginM [ClsInst]
findInstancesInScope cls = do
  instEnvs <- TcPluginM.getInstEnvs
  return $ classInstances instEnvs cls

-- | Check if there are any supermonad instances that clearly 
--   do not belong to a specific supermonad.
checkSupermonadInstances 
  :: (Class, [ClsInst]) -- ^ The @Bind@ class and instances.
  -> (Class, [ClsInst]) -- ^ The @Applicative@ class and instances.
  -> (Class, [ClsInst]) -- ^ The @Return@ class and instances.
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
  :: (Class, [ClsInst]) -- ^ The @Bind@ class and instances.
  -> (Class, [ClsInst]) -- ^ The @Applicative@ class and instances.
  -> (Class, [ClsInst]) -- ^ The @Return@ class and instances.
  -> TcPluginM (SupermonadDict, [(TyCon, SDoc)])
  -- ^ Association between type constructors and their supermonad instances.
findSupermonads (bindCls, bindInsts) (applicativeCls, applicativeInsts) (returnCls, returnInsts) = do
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









