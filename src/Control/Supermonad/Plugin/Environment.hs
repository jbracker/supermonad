
-- | Provides the plugins monadic envionment,
--   access to the environment and message printing capabilities.
module Control.Supermonad.Plugin.Environment
  ( -- * Supermonad Plugin Monad
    SupermonadPluginM
  , runSupermonadPlugin
  , runTcPlugin
    -- * Supermonad Plugin Environment Access
  , initSupermonadPlugin
  , getBindClass, getApplicativeClass, getReturnClass
  , getGivenConstraints, getWantedConstraints
  , getInstEnvs
  , getClassDictionary
  , getCustomState, putCustomState, modifyCustomState
  , getBindInstances
  , getSupermonadFor
  , getSupermonadBindFor, getSupermonadApplicativeFor, getSupermonadReturnFor
  , addTypeEqualities, addTypeEquality
  , addTyVarEqualities, addTyVarEquality
  , getTypeEqualities, getTyVarEqualities
  , whenNoResults
  , addWarning, displayWarnings
  , throwPluginError, throwPluginErrorSDoc, catchPluginError
    -- * Debug and Error Output
  , assert, assertM
  , printErr, printMsg, printObj, printWarn
  , printConstraints
  ) where

import Data.Maybe ( fromJust )
import Data.List ( groupBy )

import Control.Monad ( join, unless, forM_ )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.State  ( StateT , runStateT , gets, modify )
import Control.Monad.Except ( ExceptT, runExceptT, throwError, catchError )
import Control.Monad.Trans.Class ( lift )

import Class ( Class )
-- import Module ( Module )
import InstEnv ( InstEnvs, ClsInst )
import Type ( TyVar, Type )
import TyCon ( TyCon )
import TcRnTypes ( Ct )
import TcPluginM ( TcPluginM, tcPluginIO )
import qualified TcPluginM
import Outputable ( Outputable )
import SrcLoc ( srcSpanFileName_maybe )
import FastString ( unpackFS )
import qualified Outputable as O

import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Names 
  ( PluginClassName
  , bindClassName
  , returnClassName
  , applicativeClassName )
import Control.Supermonad.Plugin.Constraint
  ( GivenCt, WantedCt
  , constraintSourceLocation )
import Control.Supermonad.Plugin.Detect
  ( BindInst, ApplicativeInst, ReturnInst
  , findModuleByQuery, supermonadModuleQuery
  , findClassesAndInstancesInScope, supermonadClassQuery
  , findSupermonads
  , checkSupermonadInstances
  , findClassAndInstancesInScope, isClass )
import Control.Supermonad.Plugin.Utils 
  ( errIndent
  , t1st, t2nd, t3rd )
import Control.Supermonad.Plugin.ClassDict
  ( ClassDict
  , insertDict, lookupDict, emptyDict
  , lookupDictClass, lookupDictInstances )
import Control.Supermonad.Plugin.Dict
  ( InstanceDict, lookupInstDict )

-- -----------------------------------------------------------------------------
-- Plugin Monad
-- -----------------------------------------------------------------------------

-- | The error type used as result if the plugin fails.
type SupermonadError = O.SDoc

-- | The plugin monad.
type SupermonadPluginM s = ReaderT SupermonadPluginEnv 
                       ( StateT  (SupermonadPluginState s)
                       ( ExceptT SupermonadError TcPluginM
                       ) )

-- | The read-only environent of the plugin.
data SupermonadPluginEnv = SupermonadPluginEnv
  { smEnvGivenConstraints  :: [GivenCt]
  -- ^ The given and derived constraints (all of them).
  , smEnvWantedConstraints :: [WantedCt]
  -- ^ The wanted constraints (all of them).
  , smEnvClassDictionary :: ClassDict
  -- ^ Class dictionary of the environment.
  }

-- | The modifiable state of the plugin.
data SupermonadPluginState s = SupermonadPluginState 
  { smStateTyVarEqualities :: [(Ct, TyVar, Type)]
  -- ^ Equalities between type variables and types that have been derived by the plugin.
  , smStateTypeEqualities :: [(Ct, Type, Type)]
  -- ^ Eqaulities between types that have been derived by the plugin.
  , smStateWarningQueue :: [(String, O.SDoc)]
  -- ^ A queue of warnings that are only displayed if no progress could be made.
  , smStateCustom :: s
  -- ^ Custom state of the environment.
  }

initSupermonadPlugin :: SupermonadPluginM () (ClassDict, InstanceDict)
initSupermonadPlugin = do
  -- Determine if the supermonad module is available.
  eSupermonadMdl <- runTcPlugin $ findModuleByQuery supermonadModuleQuery
  _supermonadMdl <- case eSupermonadMdl of
    Right smMdl -> return smMdl
    Left mdlErrMsg -> throwPluginErrorSDoc mdlErrMsg
  
  -- Find the supermonad classes and instances.
  eFoundClsInsts <- runTcPlugin $ findClassesAndInstancesInScope supermonadClassQuery
  foundClsInsts <- case eFoundClsInsts of
    Right clsInsts -> return clsInsts
    Left errMsg -> throwPluginErrorSDoc errMsg
  
  -- Construct the initialized class dictionary.
  oldClsDict <- getClassDictionary
  let newClsDict = foldr (\(clsName, cls, clsInsts) -> insertDict clsName cls clsInsts) oldClsDict foundClsInsts 
  
  -- Calculate the supermonads in scope and check for rogue bind and return instances.
  (smInsts, smErrors) <- case (lookupDict bindClassName newClsDict, lookupDict applicativeClassName newClsDict, lookupDict returnClassName newClsDict) of
      (Just bindClsInsts, Just applicativeClsInsts, Just returnClsInsts) -> do
        (smInsts, smErrors) <- runTcPlugin $ findSupermonads bindClsInsts applicativeClsInsts returnClsInsts
        smCheckErrors <- runTcPlugin $ checkSupermonadInstances bindClsInsts applicativeClsInsts returnClsInsts
        return $ (smInsts, fmap snd smErrors ++ fmap snd smCheckErrors) 
      (_, _, _) -> return mempty
  
  -- Try to construct the environment or throw errors
  case smErrors of
    [] -> return (newClsDict, smInsts)
    _ -> do
      throwPluginErrorSDoc $ O.hang (O.text "Problems when finding supermonad instances:") errIndent $ O.vcat smErrors

-- | Runs the given supermonad plugin solver within the type checker plugin 
--   monad.
runSupermonadPlugin 
  :: [GivenCt] -- ^ /Given/ and /derived/ constraints. 
  -> [WantedCt] -- ^ /Wanted/ constraints.
  -> SupermonadPluginM () (ClassDict, s) -- ^ Initialize the custom state of the plugin.
  -> SupermonadPluginM s a -- ^ Plugin code to run.
  -> TcPluginM (Either SupermonadError a) -- ^ Either an error message or an actual plugin result.
runSupermonadPlugin givenCts wantedCts initStateM pluginM = do
  -- Try to construct the environment or throw errors
  let initEnv = SupermonadPluginEnv
        { smEnvGivenConstraints  = givenCts
        , smEnvWantedConstraints = wantedCts
        , smEnvClassDictionary   = emptyDict
        }
  let initState :: SupermonadPluginState ()
      initState = SupermonadPluginState 
        { smStateTyVarEqualities = []
        , smStateTypeEqualities  = []
        , smStateWarningQueue    = []
        , smStateCustom = ()
        }
  eInitResult <- runExceptT $ flip runStateT initState $ runReaderT initStateM initEnv
  case eInitResult of
    Left err -> return $ Left err
    Right ((smDict, customState), postInitState) -> do
      let env = initEnv { smEnvClassDictionary = smDict }
      let -- state :: SupermonadPluginState s
          state = SupermonadPluginState 
            { smStateTyVarEqualities = smStateTyVarEqualities postInitState
            , smStateTypeEqualities  = smStateTypeEqualities  postInitState
            , smStateWarningQueue    = smStateWarningQueue    postInitState
            , smStateCustom = customState
            }
      eResult <- runExceptT $ flip runStateT state $ runReaderT pluginM env
      return $ case eResult of
        Left  err -> Left err
        Right (a, _res) -> Right a


-- | Execute the given 'TcPluginM' computation within the plugin monad.
runTcPlugin :: TcPluginM a -> SupermonadPluginM s a
runTcPlugin = lift . lift . lift

-- -----------------------------------------------------------------------------
-- Plugin Environment Access
-- -----------------------------------------------------------------------------

-- | Returns the type class dictionary.
getClassDictionary :: SupermonadPluginM s ClassDict
getClassDictionary = asks smEnvClassDictionary

-- | Returns the plugins custom state.
getCustomState :: SupermonadPluginM s s
getCustomState = gets smStateCustom

-- | Writes the plugins custom state.
putCustomState :: s -> SupermonadPluginM s ()
putCustomState newS = modify (\s -> s { smStateCustom = newS })

-- | Modifies the plugins custom state.
modifyCustomState :: (s -> s) -> SupermonadPluginM s ()
modifyCustomState sf = modify (\s -> s { smStateCustom = sf (smStateCustom s) })

-- | Looks up a class by its name in the class dictionary of the 
--   plugin environment.
getClass :: PluginClassName -> SupermonadPluginM s (Maybe Class)
getClass clsName = lookupDictClass clsName <$> asks smEnvClassDictionary

-- | Returns the 'Control.Supermonad.Bind' class.
getBindClass :: SupermonadPluginM s Class
getBindClass = (fromJust . lookupDictClass bindClassName) <$> asks smEnvClassDictionary

-- | Returns the 'Control.Supermonad.Applicative' class.
getApplicativeClass :: SupermonadPluginM s Class
getApplicativeClass = (fromJust . lookupDictClass applicativeClassName) <$> asks smEnvClassDictionary

-- | Returns the 'Control.Supermonad.Return' class.
getReturnClass :: SupermonadPluginM s Class
getReturnClass = (fromJust . lookupDictClass returnClassName) <$> asks smEnvClassDictionary

-- | Returns all of the /given/ and /derived/ constraints of this plugin call.
getGivenConstraints :: SupermonadPluginM s [GivenCt]
getGivenConstraints = asks smEnvGivenConstraints

-- | Returns all of the wanted constraints of this plugin call.
getWantedConstraints :: SupermonadPluginM s [WantedCt]
getWantedConstraints = asks smEnvWantedConstraints

-- | Returns all bind instances including those given by
--   'getBindApplyInstance' and 'getBindFunctorInstance'.
getBindInstances :: SupermonadPluginM s [ClsInst]
getBindInstances = (fromJust . lookupDictInstances bindClassName) <$> asks smEnvClassDictionary

-- | Shortcut to access the instance environments.
getInstEnvs :: SupermonadPluginM s InstEnvs
getInstEnvs = runTcPlugin TcPluginM.getInstEnvs

-- | Retrieves the supermonad instances of the given type constructor,
--   if the type constructor represents a supermonad in scope.
getSupermonadFor :: TyCon -> SupermonadPluginM InstanceDict (Maybe (Maybe BindInst, ApplicativeInst, ReturnInst))
getSupermonadFor tc = do
  mBindCls        <- getClass bindClassName
  mReturnCls      <- getClass returnClassName
  mApplicativeCls <- getClass applicativeClassName
  instDict <- getCustomState
  return $ do
    bindCls         <- mBindCls
    returnCls      <- mReturnCls
    applicativeCls <- mApplicativeCls
    let bindInst     = lookupInstDict tc bindCls instDict
    applicativeInst <- lookupInstDict tc applicativeCls instDict
    returnInst      <- lookupInstDict tc returnCls instDict
    return (bindInst, applicativeInst, returnInst)
  
-- | Retrieves the supermonad 'Control.Supermonad.Bind' instances 
--   of the given type constructor, if the type constructor represents 
--   a supermonad in scope and there is a bind instance for that type constructor.
getSupermonadBindFor :: TyCon -> SupermonadPluginM InstanceDict (Maybe BindInst)
getSupermonadBindFor tc = fmap (join . fmap t1st) $ getSupermonadFor tc

-- | Retrieves the supermonad 'Control.Supermonad.Applicative' instances 
--   of the given type constructor, if the type constructor represents 
--   a supermonad in scope.
getSupermonadApplicativeFor :: TyCon -> SupermonadPluginM InstanceDict (Maybe ApplicativeInst)
getSupermonadApplicativeFor tc = fmap (fmap t2nd) $ getSupermonadFor tc

-- | Retrieves the supermonad 'Control.Supermonad.Return' instances 
--   of the given type constructor, if the type constructor represents 
--   a supermonad in scope.
getSupermonadReturnFor :: TyCon -> SupermonadPluginM InstanceDict (Maybe ReturnInst)
getSupermonadReturnFor tc = fmap (fmap t3rd) $ getSupermonadFor tc

-- | Add another type variable equality to the derived equalities.
addTyVarEquality :: Ct -> TyVar -> Type -> SupermonadPluginM s ()
addTyVarEquality ct tv ty = modify $ \s -> s { smStateTyVarEqualities = (ct, tv, ty) : smStateTyVarEqualities s }

-- | Add a list of type variable equalities to the derived equalities.
addTyVarEqualities :: [(Ct, TyVar, Type)] -> SupermonadPluginM s ()
addTyVarEqualities = mapM_ (\(ct, tv, ty) -> addTyVarEquality ct tv ty)

-- | Add another type equality to the derived equalities.
addTypeEquality :: Ct -> Type -> Type -> SupermonadPluginM s ()
addTypeEquality ct ta tb = modify $ \s -> s { smStateTypeEqualities = (ct, ta, tb) : smStateTypeEqualities s }

-- | Add a list of type equality to the derived equalities.
addTypeEqualities :: [(Ct, Type, Type)] -> SupermonadPluginM s ()
addTypeEqualities = mapM_ (\(ct, ta, tb) -> addTypeEquality ct ta tb)

-- | Returns all derived type variable equalities that were added to the results thus far.
getTyVarEqualities :: SupermonadPluginM s [(Ct, TyVar, Type)]
getTyVarEqualities = gets $ smStateTyVarEqualities

-- | Returns all derived type variable equalities that were added to the results thus far.
getTypeEqualities :: SupermonadPluginM s [(Ct, Type, Type)]
getTypeEqualities = gets $ smStateTypeEqualities

-- | Add a warning to the queue of warnings that will be displayed when no progress could be made.
addWarning :: String -> O.SDoc -> SupermonadPluginM s ()
addWarning msg details = modify $ \s -> s { smStateWarningQueue = (msg, details) : smStateWarningQueue s }

-- | Execute the given plugin code only if no plugin results were produced so far.
whenNoResults :: SupermonadPluginM s () -> SupermonadPluginM s ()
whenNoResults m = do
  tyVarEqs <- getTyVarEqualities
  tyEqs <- getTypeEqualities
  if null tyVarEqs && null tyEqs 
    then m 
    else return ()

-- | Displays the queued warning messages if no progress has been made.
displayWarnings :: SupermonadPluginM s ()
displayWarnings = whenNoResults $ do
  warns <- gets smStateWarningQueue
  forM_ warns $ \(msg, details) -> do
    printWarn msg
    internalPrint $ L.smObjMsg $ L.sDocToStr details

-- -----------------------------------------------------------------------------
-- Plugin debug and error printing
-- -----------------------------------------------------------------------------

stringToSupermonadError :: String -> SupermonadError
stringToSupermonadError = O.text

-- | Assert the given condition. If the condition does not
--   evaluate to 'True', an error with the given message will
--   be thrown the plugin aborts.
assert :: Bool -> String -> SupermonadPluginM s ()
assert cond msg = unless cond $ throwPluginError msg

-- | Assert the given condition. Same as 'assert' but with
--   a monadic condition.
assertM :: SupermonadPluginM s Bool -> String -> SupermonadPluginM s ()
assertM condM msg = do
  cond <- condM
  assert cond msg

-- | Throw an error with the given message in the plugin.
--   This will abort all further actions.
throwPluginError :: String -> SupermonadPluginM s a
throwPluginError = throwError . stringToSupermonadError

-- | Throw an error with the given message in the plugin.
--   This will abort all further actions.
throwPluginErrorSDoc :: O.SDoc -> SupermonadPluginM s a
throwPluginErrorSDoc = throwError

-- | Catch an error that was thrown by the plugin.
catchPluginError :: SupermonadPluginM s a -> (SupermonadError -> SupermonadPluginM s a) -> SupermonadPluginM s a
catchPluginError = catchError

-- | Print some generic outputable object from the plugin (Unsafe).
printObj :: Outputable o => o -> SupermonadPluginM s ()
printObj = internalPrint . L.smObjMsg . L.pprToStr

-- | Print a message from the plugin.
printMsg :: String -> SupermonadPluginM s ()
printMsg = internalPrint . L.smDebugMsg

-- | Print an error message from the plugin.
printErr :: String -> SupermonadPluginM s ()
printErr = internalPrint . L.smErrMsg

-- | Print a warning message from the plugin.
printWarn :: String -> SupermonadPluginM s ()
printWarn = internalPrint . L.smWarnMsg

-- | Internal function for printing from within the monad.
internalPrint :: String -> SupermonadPluginM s ()
internalPrint = runTcPlugin . tcPluginIO . putStr

-- | Print the given string as if it was an object. This allows custom
--   formatting of object.
printFormattedObj :: String -> SupermonadPluginM s ()
printFormattedObj = internalPrint . L.smObjMsg

-- | Print the given constraints in the plugins custom format.
printConstraints :: [Ct] -> SupermonadPluginM s ()
printConstraints cts =
  forM_ groupedCts $ \(file, ctGroup) -> do
    printFormattedObj $ maybe "From unknown file:" (("From " ++) . (++":") . unpackFS) file
    mapM_ (printFormattedObj . L.formatConstraint) ctGroup
  where
    groupedCts = (\ctGroup -> (getCtFile $ head ctGroup, ctGroup)) <$> groupBy eqFileName cts
    eqFileName ct1 ct2 = getCtFile ct1 == getCtFile ct2
    getCtFile = srcSpanFileName_maybe . constraintSourceLocation
