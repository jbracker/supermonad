
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
  , getIdentityTyCon, getIdentityModule
  , getGivenConstraints, getWantedConstraints
  , setWantedConstraints
  , getBindApplyInstance, getBindFunctorInstance
  , getCurrentResults
  , getInstEnvs
  , getBindInstances
  , addEvidenceResult, addEvidenceResults
  , addDerivedResult, addDerivedResults
  , processAndRemoveWantedConstraints
  , processEachWantedConstraint
  , whenNoResults
  , throwPluginError, throwPluginErrorSDoc, catchPluginError
    -- * Debug and Error Output
  , assert, assertM
  , printErr, printMsg, printObj
  , printConstraints
  ) where

import Data.List ( groupBy )
import Data.Monoid ( (<>) )

import Control.Arrow ( (***) )
import Control.Monad ( unless, forM, forM_ )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.State  ( StateT , runStateT , gets, get, put, modify )
import Control.Monad.Except ( ExceptT, runExceptT, throwError, catchError )
import Control.Monad.Trans.Class ( lift )

import Class ( Class )
import Module ( Module )
import InstEnv ( InstEnvs, ClsInst )
import TyCon ( TyCon )
import TcRnTypes ( Ct, TcPluginResult(..) )
import TcPluginM ( TcPluginM, tcPluginIO )
import TcEvidence ( EvTerm )
import qualified TcPluginM
import Outputable ( Outputable )
import SrcLoc ( srcSpanFileName_maybe )
import FastString ( unpackFS )
import qualified Outputable as O

import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Utils
  ( partitionM )
import Control.Supermonad.Plugin.Constraint
  ( GivenCt, WantedCt, DerivedCt
  , constraintSourceLocation )
import Control.Supermonad.Plugin.Detect
  ( findSupermonadModule
  , findBindClass, findReturnClass
  , findIdentityModule, findIdentityTyCon
  , findFunctorBindInstances
  , findInstancesInScope
  , supermonadModuleName, identityModuleName
  , bindClassName, returnClassName
  , identityTyConName )

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
  , smEnvIdentityModule :: Module
  -- ^ The 'Data.Functor.Identity' module.
  , smEnvIdentityTyCon :: TyCon
  -- ^ The 'Identity' type constructor.
  , smEnvBindFunctorInstance :: ClsInst
  -- ^ The first functor bind instance: Bind m Identity m.
  , smEnvBindApplyInstance :: ClsInst
  -- ^ The second functor bind instance: Bind Identity m m.
  , smEnvBindInstances :: [ClsInst]
  }

-- | The write-only result of the plugin.
data SupermonadPluginResult = SupermonadPluginResult 
  { smResultEvidence :: [(EvTerm, Ct)]
  -- ^ The produced result evidence of the plugin.
  , smResultDerived  :: [Ct]
  -- ^ The produced derived constraints produced by the plugin.
  }

instance Monoid SupermonadPluginResult where
  mappend a b = SupermonadPluginResult 
    { smResultEvidence = smResultEvidence a ++ smResultEvidence b
    , smResultDerived  = smResultDerived  a ++ smResultDerived  b }
  mempty = SupermonadPluginResult 
    { smResultEvidence = []
    , smResultDerived  = [] }

--isEmptyResult :: SupermonadPluginResult -> Bool
--isEmptyResult res = null (smResultDerived res) && null (smResultEvidence res)

-- | The modifiable state of the plugin.
data SupermonadPluginState = SupermonadPluginState 
  { smStateGivenConstraints  :: [GivenCt]
  -- ^ The given and derived constraints (all of them).
  , smStateWantedConstraints :: [WantedCt]
  -- ^ The wanted constraints (all of them).
  , smStateResult :: SupermonadPluginResult
  -- ^ The current results of the supermonad plugin.
  }

-- | @runPmPlugin givenAndDerived wanted m@ runs the given polymonad plugin solver @m@
--   within the type checker plugin monad. The /given/ and /derived/ constraints are
--   passed in through @givenAndDerived@ and the /wanted/ constraints are passed in
--   through @wanted@.
--
--   The function will make sure that only the polymonad constraints
--   and actually /given/, /derived/ or /wanted/ constraints
--   are kept, respectivly.
runSupermonadPlugin :: [GivenCt] -> [WantedCt] -> SupermonadPluginM a -> TcPluginM (Either SupermonadError TcPluginResult)
runSupermonadPlugin givenCts wantedCts smM = do
  mSupermonadMdl <- findSupermonadModule
  mBindCls <- findBindClass
  mReturnCls <- findReturnClass
  mIdMdl <- findIdentityModule
  mIdTyCon <- findIdentityTyCon
  mFuncBindInsts <- case (mBindCls, mIdTyCon) of
    (Just bindCls, Just idTyCon) -> findFunctorBindInstances bindCls idTyCon
    _ -> return Nothing
  case (mSupermonadMdl, mBindCls, mReturnCls, mIdMdl, mIdTyCon, mFuncBindInsts) of
    (Right supermonadMdl, Just bindCls, Just returnCls, Right idMdl, Just idTyCon, Just (bindInstFunc, bindInstApply)) -> do
      let initState = SupermonadPluginState 
            { smStateGivenConstraints  = givenCts
            , smStateWantedConstraints = wantedCts
            , smStateResult = mempty }
      bindInsts <- findInstancesInScope bindCls
      eResult <- runExceptT $ flip runStateT initState $ runReaderT smM $ SupermonadPluginEnv
        { smEnvSupermonadModule  = supermonadMdl
        , smEnvBindClass         = bindCls
        , smEnvReturnClass       = returnCls
        , smEnvIdentityModule    = idMdl
        , smEnvIdentityTyCon     = idTyCon
        , smEnvBindFunctorInstance = bindInstFunc
        , smEnvBindApplyInstance   = bindInstApply
        , smEnvBindInstances       = bindInsts }
      return $ case eResult of
        Left  err -> Left err
        Right (_a, res) -> Right $ TcPluginOk (smResultEvidence $ smStateResult res) (smResultDerived $ smStateResult res)
    (Left mdlErrMsg, _, _, _, _, _) -> do
      let msg = "Could not find " ++ supermonadModuleName ++ " module:"
      L.printErr msg
      L.printErr mdlErrMsg
      return $ Left $ stringToSupermonadError $ msg ++ " " ++ mdlErrMsg
    (_, Nothing, _, _, _, _) -> do
      let msg = "Could not find " ++ bindClassName ++ " class!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg
    (_, _, Nothing, _, _, _) -> do
      let msg = "Could not find " ++ returnClassName ++ " class!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg
    (_, _, _, Left mdlErrMsg, _, _) -> do
      let msg = "Could not find " ++ identityModuleName ++ " module:"
      L.printErr msg
      L.printErr mdlErrMsg
      return $ Left $ stringToSupermonadError $ msg ++ " " ++ mdlErrMsg
    (_, _, _, _, Nothing, _) -> do
      let msg = "Could not find " ++ identityTyConName ++ " type constructor!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg
    (_, _, _, _, _, Nothing) -> do
      let msg = "Could not find functor bind instances!"
      L.printErr msg
      return $ Left $ stringToSupermonadError msg


-- | Execute the given 'TcPluginM' computation within the polymonad plugin monad.
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

-- | Returns the module that contains the 'Data.Functor.Identity' data type.
getIdentityModule :: SupermonadPluginM Module
getIdentityModule = asks smEnvIdentityModule

-- | Returns the type constructor of the 'Data.Functor.Identity' data type.
getIdentityTyCon :: SupermonadPluginM TyCon
getIdentityTyCon = asks smEnvIdentityTyCon

-- | Returns all of the /given/ and /derived/ constraints of this plugin call.
getGivenConstraints :: SupermonadPluginM [GivenCt]
getGivenConstraints = gets smStateGivenConstraints

-- | Returns all of the wanted constraints of this plugin call.
getWantedConstraints :: SupermonadPluginM [WantedCt]
getWantedConstraints = gets smStateWantedConstraints

-- | Returns the functor bind instance: @Bind m Identity m@.
getBindFunctorInstance :: SupermonadPluginM ClsInst
getBindFunctorInstance = asks smEnvBindFunctorInstance

-- | Returns the functor apply bind instance: @Bind Identity m m@.
getBindApplyInstance :: SupermonadPluginM ClsInst
getBindApplyInstance = asks smEnvBindApplyInstance

-- | Returns all bind instances including those given by
--   'getBindApplyInstance' and 'getBindFunctorInstance'.
getBindInstances :: SupermonadPluginM [ClsInst]
getBindInstances = asks smEnvBindInstances

-- | Updates the wanted constraints that still require solving.
setWantedConstraints :: [WantedCt] -> SupermonadPluginM ()
setWantedConstraints wantedCts = do 
  state <- get
  put $ state { smStateWantedConstraints = wantedCts }

-- | Shortcut to access the instance environments.
getInstEnvs :: SupermonadPluginM InstEnvs
getInstEnvs = runTcPlugin TcPluginM.getInstEnvs

-- | Returns all collected results of the plugin so far.
getCurrentResults :: SupermonadPluginM ([(EvTerm, WantedCt)], [DerivedCt])
getCurrentResults = (\res -> (smResultEvidence res, smResultDerived res)) <$> gets smStateResult

-- | Add the given evidence to the list of results.
addEvidenceResult :: (EvTerm, WantedCt) -> SupermonadPluginM ()
addEvidenceResult evidence = addEvidenceResults [evidence]

-- | Add the given evidence to the list of results.
addEvidenceResults :: [(EvTerm, WantedCt)] -> SupermonadPluginM ()
addEvidenceResults evidence = modify $ \s -> s { smStateResult = smStateResult s <> (SupermonadPluginResult evidence []) } 

-- | Add the given derived result to the list of results.
addDerivedResult :: DerivedCt -> SupermonadPluginM ()
addDerivedResult derived = addDerivedResults [derived]

-- | Add the given derived results to the list of results.
addDerivedResults :: [DerivedCt] -> SupermonadPluginM ()
addDerivedResults derived = modify $ \s -> s { smStateResult = smStateResult s <> (SupermonadPluginResult [] derived) }

-- | Filters the wanted constraints using the given predicate. The found constraints
--   are then processed using the given function and removed from the pool
--   of wanted constraints once they were processed.
processAndRemoveWantedConstraints 
  :: (WantedCt -> SupermonadPluginM Bool) -- ^ Predicate to filter constraints with.
  -> (WantedCt -> SupermonadPluginM ([(EvTerm, WantedCt)], [Ct])) -- ^ Processing for found constraints.
  -> SupermonadPluginM ()
processAndRemoveWantedConstraints predicate process = do
  wantedCts <- getWantedConstraints
  (foundCts, restCts) <- partitionM predicate wantedCts
  forM_ foundCts $ \wantedCt -> do
    (evidence, derived) <- process wantedCt
    addEvidenceResults evidence
    addDerivedResults  derived
  setWantedConstraints restCts

-- | Goes over each wanted constraint and applies the processing function.
--   If the processing function returns true the constraint is discarded,
--   otherwise it is kept for further processing. The processing function 
--   has to add the results manually using 'addEvidenceResult' and 'addDerivedResult'.
processEachWantedConstraint :: (WantedCt -> SupermonadPluginM Bool) -> SupermonadPluginM ()
processEachWantedConstraint process = do
  wantedCts <- getWantedConstraints
  keepCts <- forM wantedCts process
  setWantedConstraints $ fmap snd $ filter (not . fst) $ zip keepCts wantedCts

-- | Execute the given plugin code only if no plugin results were produced so far.
whenNoResults :: SupermonadPluginM () -> SupermonadPluginM ()
whenNoResults m = do
  empty <- (uncurry (&&) . (null *** null)) <$> getCurrentResults
  if empty then m else return ()

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
printObj = internalPrint . L.pmObjMsg . L.pprToStr

-- | Print a message from the plugin.
printMsg :: String -> SupermonadPluginM ()
printMsg = internalPrint . L.pmDebugMsg

-- | Print a error message from the plugin.
printErr :: String -> SupermonadPluginM ()
printErr = internalPrint . L.pmErrMsg

-- | Internal function for printing from within the monad.
internalPrint :: String -> SupermonadPluginM ()
internalPrint = runTcPlugin . tcPluginIO . putStr

-- | Print the given string as if it was an object. This allows custom
--   formatting of object.
printFormattedObj :: String -> SupermonadPluginM ()
printFormattedObj = internalPrint . L.pmObjMsg

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
