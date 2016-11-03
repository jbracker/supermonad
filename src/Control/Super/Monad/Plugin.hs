
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Super.Monad.Plugin
  ( plugin ) where

import Data.Maybe ( isJust, isNothing, fromJust )
import Data.Foldable ( foldrM )
import Control.Monad ( forM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import Outputable ( SDoc, hang, text, vcat, ($$) )
import qualified Outputable as O
import Module ( Module )

import Control.Super.Plugin.Utils ( errIndent )
--import qualified Control.Super.Plugin.Log as L
import Control.Super.Plugin.InstanceDict ( InstanceDict )
import Control.Super.Plugin.ClassDict ( ClassDict )
import Control.Super.Plugin.Solving
  ( solveConstraints )
import Control.Super.Plugin.Environment
  ( SupermonadPluginM
  , runSupermonadPluginAndReturn, runTcPlugin
  , getWantedConstraints
  , getClass, getClassDictionary
  , throwPluginErrorSDoc
  , printMsg
  -- , printObj, printConstraints
  )
import Control.Super.Plugin.Environment.Lift
  ( findClassesAndInstancesInScope )
import Control.Super.Plugin.Detect 
  ( ModuleQuery(..)
  , ClassQuery(..)
  , InstanceImplication
  , clsDictInstImp, clsDictInstEquiv
  , checkInstances
  , findModuleByQuery
  , findMonoTopTyConInstances
  , defaultFindEitherModuleErrMsg )
import Control.Super.Plugin.Names
  ( PluginClassName
  , supermonadModuleName, supermonadCtModuleName
  , legacySupermonadModuleName, legacySupermonadCtModuleName
  , supermonadPreludeModuleName, supermonadCtPreludeModuleName
  , legacySupermonadPreludeModuleName, legacySupermonadCtPreludeModuleName
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
      
      forM solvingGroups $ \solvingGroup -> do
        mClss <- forM solvingGroup $ \clsName -> do
          mCls <- getClass clsName
          return (clsName, mCls)
        if all (isJust . snd) mClss then do 
          wantedCts <- getWantedConstraints
          solveConstraints (fmap (fromJust . snd) mClss) wantedCts
        else do
          throwPluginErrorSDoc $ O.hang (O.text "Missing classes:") errIndent 
                               $ O.hcat $ O.punctuate (O.text ", ") 
                               $ fmap (O.quotes . O.text . fst) 
                               $ filter (isNothing . snd) mClss  

-- -----------------------------------------------------------------------------
-- Supermonad specific initialization
-- -----------------------------------------------------------------------------

-- | Configure which groups of classes need to be solved together.
solvingGroups :: [[PluginClassName]]
solvingGroups = 
  [ [ bindClassName, returnClassName, applicativeClassName ] -- Supermonad group
  ]

-- | Configure the classes the plugin works with.
pluginClassQueries :: [ClassQuery]
pluginClassQueries = [ supermonadClassQuery ]

-- | Configure which instance implications the plugin needs to verify.
pluginInstanceImplications :: ClassDict -> [InstanceImplication]
pluginInstanceImplications clsDict = supermonadInstanceImplications clsDict

-- | Queries the module providing the supermonad classes.
supermonadModuleQuery :: ModuleQuery
supermonadModuleQuery = EitherModule
  [ AnyModule [ ThisModule supermonadModuleName Nothing
              , ThisModule supermonadPreludeModuleName Nothing
              , ThisModule legacySupermonadModuleName Nothing
              , ThisModule legacySupermonadPreludeModuleName Nothing
              ]
  , AnyModule [ ThisModule supermonadCtModuleName Nothing
              , ThisModule supermonadCtPreludeModuleName Nothing
              , ThisModule legacySupermonadCtModuleName Nothing
              , ThisModule legacySupermonadCtPreludeModuleName Nothing
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
  
  -- Find the supermonad classes and instances and add the to the class dictionary.
  oldClsDict <- getClassDictionary
  newClsDict <- foldrM findClassesAndInstancesInScope oldClsDict pluginClassQueries
  
  -- Calculate the supermonads in scope and check for rogue bind and return instances.
  let smInsts = findMonoTopTyConInstances newClsDict
  let smErrors = fmap snd $ checkInstances newClsDict smInsts (pluginInstanceImplications newClsDict)
  
  -- Try to construct the environment or throw errors
  case smErrors of
    [] -> return (newClsDict, smInsts)
    _ -> do
      throwPluginErrorSDoc $ hang (text "Problems when finding supermonad instances:") errIndent $ vcat smErrors




