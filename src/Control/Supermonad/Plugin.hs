
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Supermonad.Plugin
  ( plugin ) where

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import Outputable ( SDoc, hang, text, vcat, ($$) )
import Module ( Module )

import Control.Supermonad.Plugin.Utils ( errIndent )
import qualified Control.Supermonad.Plugin.Log as L
import Control.Supermonad.Plugin.InstanceDict ( InstanceDict )
import Control.Supermonad.Plugin.ClassDict ( ClassDict, insertClsDict )
import Control.Supermonad.Plugin.Solving
  ( solveConstraints )
import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM
  , runSupermonadPluginAndReturn, runTcPlugin
  , getWantedConstraints
  , getClass, getClassDictionary
  , throwPluginError, throwPluginErrorSDoc
  , getTypeEqualities, getTyVarEqualities
  , printMsg
  -- , printObj, printConstraints
  )
import Control.Supermonad.Plugin.Environment.Lift
  ( findClassesAndInstancesInScope )
import Control.Supermonad.Plugin.Detect 
  ( ModuleQuery(..)
  , ClassQuery(..)
  , InstanceImplication
  , clsDictInstImp, clsDictInstEquiv
  , checkInstances
  , findModuleByQuery
  , findMonoTopTyConInstances
  , defaultFindEitherModuleErrMsg )
import Control.Supermonad.Plugin.Names
  ( supermonadModuleName, supermonadCtModuleName
  , supermonadPreludeModuleName, supermonadCtPreludeModuleName
  , bindClassName, returnClassName, applicativeClassName )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

-- | The supermonad type checker plugin for GHC.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_clOpts -> Just supermonadPlugin }

-- -----------------------------------------------------------------------------
-- Actual Plugin Code
-- -----------------------------------------------------------------------------

type SupermonadState = ()

-- | The type checker plugin.
supermonadPlugin :: TcPlugin
supermonadPlugin = TcPlugin
  { tcPluginInit  = supermonadInit
  , tcPluginSolve = supermonadSolve
  , tcPluginStop  = supermonadStop
  }

-- | No initialization needs takes place.
supermonadInit :: TcPluginM SupermonadState
supermonadInit = return ()

-- | No clean up needs to take place.
supermonadStop :: SupermonadState -> TcPluginM ()
supermonadStop _s = return ()

-- | The plugin code wrapper. Handles execution of the monad stack.
supermonadSolve :: SupermonadState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
supermonadSolve _s given derived wanted = do
  runSupermonadPluginAndReturn (given ++ derived) wanted initSupermonadPlugin $ do
      printMsg "Invoke supermonad plugin..."
      
      mBindCls <- getClass bindClassName
      mReturnCls <- getClass returnClassName
      mApplicativeCls <- getClass applicativeClassName
      
      case (mBindCls, mReturnCls, mApplicativeCls) of
        
        (Just bindCls, Just returnCls, Just applicativeCls) -> do
          wantedCts <- getWantedConstraints
          solveConstraints [bindCls, returnCls, applicativeCls] wantedCts
        
        _ -> throwPluginError "Missing 'Bind', 'Return' or 'Applicative' class!"

-- -----------------------------------------------------------------------------
-- Supermonad specific initialization
-- -----------------------------------------------------------------------------

-- | Queries the module providing the supermonad classes.
supermonadModuleQuery :: ModuleQuery
supermonadModuleQuery = EitherModule
  [ AnyModule [ ThisModule supermonadModuleName Nothing
              , ThisModule supermonadPreludeModuleName Nothing
              ]
  , AnyModule [ ThisModule supermonadCtModuleName Nothing
              , ThisModule supermonadCtPreludeModuleName Nothing
              ]
  ] $ Just findSupermonadModulesErrMsg

-- | Queries the supermonad classes.
supermonadClassQuery :: ClassQuery
supermonadClassQuery = ClassQuery supermonadModuleQuery 
  [ (bindClassName       , 3)
  , (returnClassName     , 1)
  , (applicativeClassName, 3)
  ]

-- | Ensures that all supermonad instance implications with the group of 
--   one type constructor are obeyed.
supermonadInstanceImplications :: ClassDict -> [InstanceImplication]
supermonadInstanceImplications clsDict =
    (applicativeClassName <=> returnClassName) ++
    (bindClassName        ==> returnClassName)
  where
    (==>) = clsDictInstImp clsDict
    (<=>) = clsDictInstEquiv clsDict

-- | Function to produce proper error messages in the module query.
findSupermonadModulesErrMsg :: [Either SDoc Module] -> SDoc
findSupermonadModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find supermonad or constrained supermonad modules!") errIndent (errA $$ errB)
findSupermonadModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained supermonad modules!"
findSupermonadModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls

-- | Initialize the plugin environment.
initSupermonadPlugin :: SupermonadPluginM () (ClassDict, InstanceDict)
initSupermonadPlugin = do
  -- Determine if the supermonad module is available.
  eSupermonadMdl <- runTcPlugin $ findModuleByQuery supermonadModuleQuery
  _supermonadMdl <- case eSupermonadMdl of
    Right smMdl -> return smMdl
    Left mdlErrMsg -> throwPluginErrorSDoc mdlErrMsg
  
  -- Find the supermonad classes and instances.
  newClsDict <- findClassesAndInstancesInScope supermonadClassQuery
  
  -- Calculate the supermonads in scope and check for rogue bind and return instances.
  let smInsts = findMonoTopTyConInstances newClsDict
  let smErrors = fmap snd $ checkInstances newClsDict smInsts (supermonadInstanceImplications newClsDict)
  
  -- Try to construct the environment or throw errors
  case smErrors of
    [] -> return (newClsDict, smInsts)
    _ -> do
      throwPluginErrorSDoc $ hang (text "Problems when finding supermonad instances:") errIndent $ vcat smErrors




