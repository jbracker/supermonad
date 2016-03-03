
-- | Provides the polymonad plugin monadic envionment,
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
  , getCurrentResults
  , getInstEnvs
  , addEvidenceResult, addEvidenceResults
  , addDerivedResult, addDerivedResults
  , processAndRemoveWantedConstraints
  , whenNoResults
  , throwPluginError
    -- * Debug and Error Output
  , assert, assertM
  , printErr, printMsg, printObj
  , printConstraints
  ) where

import Data.List ( groupBy )
import Data.Monoid ( (<>) )

import Control.Arrow ( (***) )
import Control.Monad ( unless, forM_ )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.State  ( StateT , runStateT , gets, get, put, modify )
import Control.Monad.Except ( ExceptT, runExceptT, throwError )
import Control.Monad.Trans.Class ( lift )

import Class ( Class )
import Module ( Module )
import InstEnv ( InstEnvs )
import TyCon ( TyCon )
import TcRnTypes ( Ct, TcPluginResult(..) )
import TcPluginM ( TcPluginM, tcPluginIO )
import TcEvidence ( EvTerm )
import qualified TcPluginM
import Outputable ( Outputable )
import SrcLoc ( srcSpanFileName_maybe )
import FastString ( unpackFS )

import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Utils
  ( partitionM )
import Control.Supermonad.Plugin.Constraint
  ( GivenCt, WantedCt
  , constraintSourceLocation )
import Control.Supermonad.Plugin.Detect
  ( findSupermonadModule
  , findBindClass, findReturnClass
  , findIdentityModule, findIdentityTyCon
  , supermonadModuleName, identityModuleName
  , bindClassName, returnClassName
  , identityTyConName )

-- -----------------------------------------------------------------------------
-- Plugin Monad
-- -----------------------------------------------------------------------------

-- | The plugin monad.
type SupermonadPluginM = ReaderT SupermonadPluginEnv 
                       ( StateT  SupermonadPluginState 
                       ( ExceptT String TcPluginM
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
runSupermonadPlugin :: [GivenCt] -> [WantedCt] -> SupermonadPluginM a -> TcPluginM (Either String TcPluginResult)
runSupermonadPlugin givenCts wantedCts smM = do
  mSupermonadMdl <- findSupermonadModule
  mBindCls <- findBindClass
  mReturnCls <- findReturnClass
  mIdMdl <- findIdentityModule
  mIdTyCon <- findIdentityTyCon
  case (mSupermonadMdl, mBindCls, mReturnCls, mIdMdl, mIdTyCon) of
    (Right supermonadMdl, Just bindCls, Just returnCls, Right idMdl, Just idTyCon) -> do
      let initState = SupermonadPluginState 
            { smStateGivenConstraints  = givenCts
            , smStateWantedConstraints = wantedCts
            , smStateResult = mempty }
      eResult <- runExceptT $ flip runStateT initState $ runReaderT smM $ SupermonadPluginEnv
        { smEnvSupermonadModule  = supermonadMdl
        , smEnvBindClass         = bindCls
        , smEnvReturnClass       = returnCls
        , smEnvIdentityModule    = idMdl
        , smEnvIdentityTyCon     = idTyCon }
      return $ case eResult of
        Left  err -> Left err
        Right (_a, res) -> Right $ TcPluginOk (smResultEvidence $ smStateResult res) (smResultDerived $ smStateResult res)
    (Left mdlErrMsg, _, _, _, _) -> do
      let msg = "Could not find " ++ supermonadModuleName ++ " module:"
      L.printErr msg
      L.printErr mdlErrMsg
      return $ Left $ msg ++ " " ++ mdlErrMsg
    (_, Nothing, _, _, _) -> do
      let msg = "Could not find " ++ bindClassName ++ " class!"
      L.printErr msg
      return $ Left msg
    (_, _, Nothing, _, _) -> do
      let msg = "Could not find " ++ returnClassName ++ " class!"
      L.printErr msg
      return $ Left msg
    (_, _, _, Left mdlErrMsg, _) -> do
      let msg = "Could not find " ++ identityModuleName ++ " module:"
      L.printErr msg
      L.printErr mdlErrMsg
      return $ Left $ msg ++ " " ++ mdlErrMsg
    (_, _, _, _, Nothing) -> do
      let msg = "Could not find " ++ identityTyConName ++ " type constructor!"
      L.printErr msg
      return $ Left msg

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

setWantedConstraints :: [WantedCt] -> SupermonadPluginM ()
setWantedConstraints wantedCts = do 
  state <- get
  put $ state { smStateWantedConstraints = wantedCts }

-- | Shortcut to access the instance environments.
getInstEnvs :: SupermonadPluginM InstEnvs
getInstEnvs = runTcPlugin TcPluginM.getInstEnvs

getCurrentResults :: SupermonadPluginM ([(EvTerm, WantedCt)], [Ct])
getCurrentResults = (\res -> (smResultEvidence res, smResultDerived res)) <$> gets smStateResult

addEvidenceResult :: (EvTerm, WantedCt) -> SupermonadPluginM ()
addEvidenceResult evidence = addEvidenceResults [evidence]

addEvidenceResults :: [(EvTerm, WantedCt)] -> SupermonadPluginM ()
addEvidenceResults evidence = modify $ \s -> s { smStateResult = smStateResult s <> (SupermonadPluginResult evidence []) } 

addDerivedResult :: Ct -> SupermonadPluginM ()
addDerivedResult derived = addDerivedResults [derived]

addDerivedResults :: [Ct] -> SupermonadPluginM ()
addDerivedResults derived = modify $ \s -> s { smStateResult = smStateResult s <> (SupermonadPluginResult [] derived) }

processAndRemoveWantedConstraints :: (WantedCt -> SupermonadPluginM Bool) -> (WantedCt -> SupermonadPluginM ([(EvTerm, WantedCt)], [Ct])) -> SupermonadPluginM ()
processAndRemoveWantedConstraints predicate process = do
  wantedCts <- getWantedConstraints
  (foundCts, restCts) <- partitionM predicate wantedCts
  forM_ foundCts $ \wantedCt -> do
    (evidence, derived) <- process wantedCt
    addEvidenceResults evidence
    addDerivedResults  derived
  setWantedConstraints restCts

whenNoResults :: SupermonadPluginM () -> SupermonadPluginM ()
whenNoResults m = do
  empty <- (uncurry (&&) . (null *** null)) <$> getCurrentResults
  if empty then m else return ()

-- listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)

-- -----------------------------------------------------------------------------
-- Plugin debug and error printing
-- -----------------------------------------------------------------------------

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
throwPluginError = throwError

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
