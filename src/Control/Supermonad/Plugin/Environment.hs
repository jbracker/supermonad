
-- | Provides the plugins monadic envionment,
--   access to the environment and message printing capabilities.
module Control.Supermonad.Plugin.Environment
  ( -- * Polymonad Plugin Monad
    SupermonadPluginM
  , runSupermonadPlugin
  , runTcPlugin
    -- * Polymonad Plugin Environment Access
  , getBindClass, getReturnClass
  , getSupermonadModule
  , getGivenConstraints, getWantedConstraints
  , getInstEnvs
  , getBindInstances
  , getSupermonadFor
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

import Data.List ( groupBy )
import Data.Map ( Map )
import qualified Data.Map as M

import Control.Monad ( unless, forM_ )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.State  ( StateT , runStateT , gets, modify )
import Control.Monad.Except ( ExceptT, runExceptT, throwError, catchError )
import Control.Monad.Trans.Class ( lift )

import Class ( Class )
import Module ( Module )
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
import Control.Supermonad.Plugin.Constraint
  ( GivenCt, WantedCt
  , constraintSourceLocation )
import Control.Supermonad.Plugin.Detect
  ( findSupermonadModule
  , findBindClass, findReturnClass
  , findInstancesInScope
  , bindClassName, returnClassName
  , findSupermonads
  , checkSupermonadInstances )

-- -----------------------------------------------------------------------------
-- Plugin Monad
-- -----------------------------------------------------------------------------

-- | The error type used as result if the plugin fails.
type SupermonadError = O.SDoc

-- | The plugin monad.
type SupermonadPluginM = ReaderT SupermonadPluginEnv 
                       ( StateT  SupermonadPluginState 
                       ( ExceptT SupermonadError TcPluginM
                       ) )

-- | The read-only environent of the plugin.
data SupermonadPluginEnv = SupermonadPluginEnv
  { smEnvSupermonadModule :: Module
  -- ^ The 'Control.Supermonad' module.
  , smEnvBindClass :: Class
  -- ^ The 'Bind' class.
  , smEnvReturnClass :: Class
  -- ^ The 'Return' class.
  , smEnvBindInstances :: [ClsInst]
  -- ^ Collection of all 'Bind' instances.
  , smEnvGivenConstraints  :: [GivenCt]
  -- ^ The given and derived constraints (all of them).
  , smEnvWantedConstraints :: [WantedCt]
  -- ^ The wanted constraints (all of them).
  , smEnvSupermonads :: Map TyCon (ClsInst, ClsInst)
  -- ^ The supermonads currently in scope. Associates the type constructor 
  --   of each supermonad with its 'Control.Supermonad.Bind' and 
  --   'Control.Supermonad.Return' instance.
  }

-- | The modifiable state of the plugin.
data SupermonadPluginState = SupermonadPluginState 
  { smStateTyVarEqualities :: [(Ct, TyVar, Type)]
  -- ^ Equalities between type variables and types that have been derived by the plugin.
  , smStateTypeEqualities :: [(Ct, Type, Type)]
  -- ^ Eqaulities between types that have been derived by the plugin.
  , smStateWarningQueue :: [(String, O.SDoc)]
  -- ^ A queue of warnings that are only displayed if no progress could be made.
  }

-- | Runs the given supermonad plugin solver within the type checker plugin 
--   monad.
runSupermonadPlugin 
  :: [GivenCt] -- ^ /Given/ and /derived/ constraints. 
  -> [WantedCt] -- ^ /Wanted/ constraints.
  -> SupermonadPluginM a -- ^ Plugin code to run.
  -> TcPluginM (Either SupermonadError a) -- ^ Either an error message or an actual plugin result.
runSupermonadPlugin givenCts wantedCts smM = do
  mSupermonadMdl <- findSupermonadModule
  mBindCls <- findBindClass
  mReturnCls <- findReturnClass
  -- Calculate the supermonads in scope and check for rogue bind and return instances.
  (smInsts, smErrors) <- case (mBindCls, mReturnCls) of
      (Just bindCls, Just returnCls) -> do
        (smInsts, smErrors) <- findSupermonads bindCls returnCls
        smCheckErrors <- checkSupermonadInstances bindCls returnCls
        return $ (smInsts, fmap snd smErrors ++ fmap snd smCheckErrors) 
      (_, _) -> return mempty
  -- Try to construct the environment or throw errors
  case (mSupermonadMdl, mBindCls, mReturnCls, smErrors) of
    (Right supermonadMdl, Just bindCls, Just returnCls, []) -> do
      let initState = SupermonadPluginState 
            { smStateTyVarEqualities = []
            , smStateTypeEqualities  = []
            , smStateWarningQueue    = [] 
            }
      bindInsts <- findInstancesInScope bindCls
      eResult <- runExceptT $ flip runStateT initState $ runReaderT smM $ SupermonadPluginEnv
        { smEnvSupermonadModule  = supermonadMdl
        , smEnvBindClass         = bindCls
        , smEnvReturnClass       = returnCls
        , smEnvBindInstances     = bindInsts
        , smEnvGivenConstraints  = givenCts
        , smEnvWantedConstraints = wantedCts
        , smEnvSupermonads       = smInsts
        }
      return $ case eResult of
        Left  err -> Left err
        Right (a, _res) -> Right a
    (Left mdlErrMsg, _, _, _) -> do
      let msg = "Could not find supermonad module:"
      L.printErr msg
      L.printErr $ L.sDocToStr mdlErrMsg
      return $ Left $ stringToSupermonadError msg O.$$ mdlErrMsg
    (_, Nothing, _, _) -> do
      let msg = "Could not find " ++ bindClassName ++ " class!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg
    (_, _, Nothing, _) -> do
      let msg = "Could not find " ++ returnClassName ++ " class!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg
    (_, _, _, _) -> do
      let msg = "Problems when finding supermonad instances:"
      let sdocErr = O.vcat smErrors
      L.printErr msg
      L.printErr $ L.sDocToStr sdocErr
      return $ Left $ stringToSupermonadError msg O.$$ sdocErr


-- | Execute the given 'TcPluginM' computation within the plugin monad.
runTcPlugin :: TcPluginM a -> SupermonadPluginM a
runTcPlugin = lift . lift . lift

-- -----------------------------------------------------------------------------
-- Plugin Environment Access
-- -----------------------------------------------------------------------------

-- | Returns the 'Control.Supermonad.Bind' class.
getBindClass :: SupermonadPluginM Class
getBindClass = asks smEnvBindClass

-- | Returns the 'Control.Supermonad.Return' class.
getReturnClass :: SupermonadPluginM Class
getReturnClass = asks smEnvReturnClass

-- | The 'Control.Supermonad' module.
getSupermonadModule :: SupermonadPluginM Module
getSupermonadModule = asks smEnvSupermonadModule

-- | Returns all of the /given/ and /derived/ constraints of this plugin call.
getGivenConstraints :: SupermonadPluginM [GivenCt]
getGivenConstraints = asks smEnvGivenConstraints

-- | Returns all of the wanted constraints of this plugin call.
getWantedConstraints :: SupermonadPluginM [WantedCt]
getWantedConstraints = asks smEnvWantedConstraints

-- | Returns all bind instances including those given by
--   'getBindApplyInstance' and 'getBindFunctorInstance'.
getBindInstances :: SupermonadPluginM [ClsInst]
getBindInstances = asks smEnvBindInstances

-- | Shortcut to access the instance environments.
getInstEnvs :: SupermonadPluginM InstEnvs
getInstEnvs = runTcPlugin TcPluginM.getInstEnvs

-- | Retrieves the supermonad bind and return instance (in that order) of the given type constructor,
--   if the type constructor represents a supermonad in scope.
getSupermonadFor :: TyCon -> SupermonadPluginM (Maybe (ClsInst, ClsInst))
getSupermonadFor tc = (return . M.lookup tc) =<< asks smEnvSupermonads

-- | Add another type variable equality to the derived equalities.
addTyVarEquality :: Ct -> TyVar -> Type -> SupermonadPluginM ()
addTyVarEquality ct tv ty = modify $ \s -> s { smStateTyVarEqualities = (ct, tv, ty) : smStateTyVarEqualities s }

-- | Add a list of type variable equalities to the derived equalities.
addTyVarEqualities :: [(Ct, TyVar, Type)] -> SupermonadPluginM ()
addTyVarEqualities = mapM_ (\(ct, tv, ty) -> addTyVarEquality ct tv ty)

-- | Add another type equality to the derived equalities.
addTypeEquality :: Ct -> Type -> Type -> SupermonadPluginM ()
addTypeEquality ct ta tb = modify $ \s -> s { smStateTypeEqualities = (ct, ta, tb) : smStateTypeEqualities s }

-- | Add a list of type equality to the derived equalities.
addTypeEqualities :: [(Ct, Type, Type)] -> SupermonadPluginM ()
addTypeEqualities = mapM_ (\(ct, ta, tb) -> addTypeEquality ct ta tb)

-- | Returns all derived type variable equalities that were added to the results thus far.
getTyVarEqualities :: SupermonadPluginM [(Ct, TyVar, Type)]
getTyVarEqualities = gets $ smStateTyVarEqualities

-- | Returns all derived type variable equalities that were added to the results thus far.
getTypeEqualities :: SupermonadPluginM [(Ct, Type, Type)]
getTypeEqualities = gets $ smStateTypeEqualities

-- | Add a warning to the queue of warnings that will be displayed when no progress could be made.
addWarning :: String -> O.SDoc -> SupermonadPluginM ()
addWarning msg details = modify $ \s -> s { smStateWarningQueue = (msg, details) : smStateWarningQueue s }

-- | Execute the given plugin code only if no plugin results were produced so far.
whenNoResults :: SupermonadPluginM () -> SupermonadPluginM ()
whenNoResults m = do
  tyVarEqs <- getTyVarEqualities
  tyEqs <- getTypeEqualities
  if null tyVarEqs && null tyEqs 
    then m 
    else return ()

-- | Displays the queued warning messages if no progress has been made.
displayWarnings :: SupermonadPluginM ()
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
assert :: Bool -> String -> SupermonadPluginM ()
assert cond msg = unless cond $ throwPluginError msg

-- | Assert the given condition. Same as 'assert' but with
--   a monadic condition.
assertM :: SupermonadPluginM Bool -> String -> SupermonadPluginM ()
assertM condM msg = do
  cond <- condM
  assert cond msg

-- | Throw an error with the given message in the plugin.
--   This will abort all further actions.
throwPluginError :: String -> SupermonadPluginM a
throwPluginError = throwError . stringToSupermonadError

-- | Throw an error with the given message in the plugin.
--   This will abort all further actions.
throwPluginErrorSDoc :: O.SDoc -> SupermonadPluginM a
throwPluginErrorSDoc = throwError

-- | Catch an error that was thrown by the plugin.
catchPluginError :: SupermonadPluginM a -> (SupermonadError -> SupermonadPluginM a) -> SupermonadPluginM a
catchPluginError = catchError

-- | Print some generic outputable object from the plugin (Unsafe).
printObj :: Outputable o => o -> SupermonadPluginM ()
printObj = internalPrint . L.smObjMsg . L.pprToStr

-- | Print a message from the plugin.
printMsg :: String -> SupermonadPluginM ()
printMsg = internalPrint . L.smDebugMsg

-- | Print an error message from the plugin.
printErr :: String -> SupermonadPluginM ()
printErr = internalPrint . L.smErrMsg

-- | Print a warning message from the plugin.
printWarn :: String -> SupermonadPluginM ()
printWarn = internalPrint . L.smWarnMsg

-- | Internal function for printing from within the monad.
internalPrint :: String -> SupermonadPluginM ()
internalPrint = runTcPlugin . tcPluginIO . putStr

-- | Print the given string as if it was an object. This allows custom
--   formatting of object.
printFormattedObj :: String -> SupermonadPluginM ()
printFormattedObj = internalPrint . L.smObjMsg

-- | Print the given constraints in the plugins custom format.
printConstraints :: [Ct] -> SupermonadPluginM ()
printConstraints cts =
  forM_ groupedCts $ \(file, ctGroup) -> do
    printFormattedObj $ maybe "From unknown file:" (("From " ++) . (++":") . unpackFS) file
    mapM_ (printFormattedObj . L.formatConstraint) ctGroup
  where
    groupedCts = (\ctGroup -> (getCtFile $ head ctGroup, ctGroup)) <$> groupBy eqFileName cts
    eqFileName ct1 ct2 = getCtFile ct1 == getCtFile ct2
    getCtFile = srcSpanFileName_maybe . constraintSourceLocation
