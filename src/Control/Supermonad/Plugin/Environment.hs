
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
  , getWantedReturnConstraints
  , getInstEnvs
  , throwPluginError
    -- * Debug and Error Output
  , assert, assertM
  , printErr, printMsg, printObj
  , printConstraints
  ) where

import Data.List ( groupBy )

import Control.Monad ( unless, forM_ )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.Trans.Writer ( WriterT, runWriterT )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.Class ( lift )

import Class ( Class )
import Module ( Module )
import InstEnv ( InstEnvs )
import TyCon ( TyCon )
import TcRnTypes ( Ct )
import TcPluginM ( TcPluginM, tcPluginIO )
import TcEvidence ( EvTerm )
import qualified TcPluginM
import Outputable ( Outputable )
import SrcLoc ( srcSpanFileName_maybe )
import FastString ( unpackFS )

import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.Constraint
  ( GivenCt, WantedCt
  , constraintSourceLocation
  , isClassConstraint )
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
type SupermonadPluginM = WriterT SupermonadPluginResult (ReaderT SupermonadPluginEnv (ExceptT String TcPluginM))

-- | The read-only environent of the plugin.
data SupermonadPluginEnv = SupermonadPluginEnv
  { smEnvSupermonadModule    :: Module
  -- ^ The 'Control.Supermonad' module.
  , smEnvBindClass     :: Class
  -- ^ The 'Bind' class.
  , smEnvReturnClass     :: Class
  -- ^ The 'Return' class.
  , smEnvIdentityModule :: Module
  -- ^ The 'Data.Functor.Identity' module.
  , smEnvIdentityTyCon  :: TyCon
  -- ^ The 'Identity' type constructor.
  , smEnvGivenConstraints  :: [GivenCt]
  -- ^ The given and derived constraints (all of them).
  , smEnvWantedConstraints :: [WantedCt]
  -- ^ The wanted constraints (all of them).
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

-- | @runPmPlugin givenAndDerived wanted m@ runs the given polymonad plugin solver @m@
--   within the type checker plugin monad. The /given/ and /derived/ constraints are
--   passed in through @givenAndDerived@ and the /wanted/ constraints are passed in
--   through @wanted@.
--
--   The function will make sure that only the polymonad constraints
--   and actually /given/, /derived/ or /wanted/ constraints
--   are kept, respectivly.
runSupermonadPlugin :: [GivenCt] -> [WantedCt] -> SupermonadPluginM a -> TcPluginM (Either String a)
runSupermonadPlugin givenCts wantedCts smM = do
  mSupermonadMdl <- findSupermonadModule
  mBindCls <- findBindClass
  mReturnCls <- findReturnClass
  mIdMdl <- findIdentityModule
  mIdTyCon <- findIdentityTyCon
  case (mSupermonadMdl, mBindCls, mReturnCls, mIdMdl, mIdTyCon) of
    (Right supermonadMdl, Just bindCls, Just returnCls, Right idMdl, Just idTyCon) -> do
      let (_, result) = runWriterT smM
      runExceptT $ runReaderT (runWriterT smMCont) $ SupermonadPluginEnv
        { smEnvSupermonadModule = supermonadMdl
        , smEnvBindClass        = bindCls
        , smEnvReturnClass      = returnCls
        , smEnvIdentityModule     = idMdl
        , smEnvIdentityTyCon    = idTyCon
        , smEnvGivenConstraints  = givenCts
        , smEnvWantedConstraints = wantedCts
        }
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

smAsks :: (SupermonadPluginEnv -> a) -> SupermonadPluginM a
smAsks = lift . asks

-- | Returns the 'Control.Supermonad.Bind' class.
getBindClass :: SupermonadPluginM Class
getBindClass = smAsks smEnvBindClass

-- | Returns the 'Control.Supermonad.Return' class.
getReturnClass :: SupermonadPluginM Class
getReturnClass = smAsks smEnvReturnClass

-- | The 'Control.Supermonad' module.
getSupermonadModule :: SupermonadPluginM Module
getSupermonadModule = smAsks smEnvSupermonadModule

-- | Returns the module that contains the 'Data.Functor.Identity' data type.
getIdentityModule :: SupermonadPluginM Module
getIdentityModule = smAsks smEnvIdentityModule

-- | Returns the type constructor of the 'Data.Functor.Identity' data type.
getIdentityTyCon :: SupermonadPluginM TyCon
getIdentityTyCon = smAsks smEnvIdentityTyCon

-- | Returns all of the /given/ and /derived/ constraints of this plugin call.
getGivenConstraints :: SupermonadPluginM [GivenCt]
getGivenConstraints = smAsks smEnvGivenConstraints

-- | Returns all of the wanted constraints of this plugin call.
getWantedConstraints :: SupermonadPluginM [WantedCt]
getWantedConstraints = smAsks smEnvWantedConstraints

-- | Shortcut to access the instance environments.
getInstEnvs :: SupermonadPluginM InstEnvs
getInstEnvs = runTcPlugin TcPluginM.getInstEnvs

getWantedReturnConstraints :: SupermonadPluginM [WantedCt]
getWantedReturnConstraints = do
  wantedCts <- getWantedConstraints
  returnCls <- getReturnClass
  return $ filter (isClassConstraint returnCls) wantedCts

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
throwPluginError = lift . lift . throwE

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
