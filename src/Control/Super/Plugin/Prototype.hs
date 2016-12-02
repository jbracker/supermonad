 
module Control.Super.Plugin.Prototype
  ( pluginPrototype
  ) where


import Data.Maybe ( isJust, isNothing, fromJust, catMaybes )
import Data.Foldable ( foldrM )
import Control.Monad ( forM )

import Plugins ( Plugin(tcPlugin), defaultPlugin )
import TcRnTypes
  ( Ct(..)
  , TcPlugin(..), TcPluginResult(..) )
import TcPluginM ( TcPluginM )
import Outputable ( hang, text, vcat )
import qualified Outputable as O

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
  , isOptionalClass
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
  , checkInstances
  , findModuleByQuery
  , findMonoTopTyConInstances )
import Control.Super.Plugin.Names ( PluginClassName )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

type SupermonadState = ()

-- | The supermonad type checker plugin for GHC.
pluginPrototype :: ModuleQuery
                -- ^ Query the modules that a mandatory for the plugin to work.
                -> [ClassQuery] 
                -- ^ The classes that the plugin will solve.
                -> [[PluginClassName]]
                -- ^ The sets of class names that require being solved together.
                -> (ClassDict -> [InstanceImplication]) 
                -- ^ The depedencies between different class instances, that 
                --   cannot be implemented using the Haskell type class definitions.
                -> Plugin
pluginPrototype mdlQuery clsQuery solvingGroups instImps = 
  defaultPlugin { tcPlugin = \_clOpts -> Just plugin }
  where
    plugin :: TcPlugin
    plugin = TcPlugin
      { tcPluginInit  = pluginInit
      , tcPluginSolve = pluginSolve
      , tcPluginStop  = pluginStop
      }
    
    -- | No initialization needs takes place.
    pluginInit :: TcPluginM SupermonadState
    pluginInit = return ()
    
    -- | No clean up needs to take place.
    pluginStop :: SupermonadState -> TcPluginM ()
    pluginStop _s = return ()

    -- | The plugin code wrapper. Handles execution of the monad stack.
    pluginSolve :: SupermonadState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
    pluginSolve _s given derived wanted = do
      runSupermonadPluginAndReturn (given ++ derived) wanted initSupermonadPlugin $ do
          printMsg "Invoke (super) plugin..."
          
          forM solvingGroups $ \solvingGroup -> do
            -- Find the classes in the solving group.
            mClss <- fmap catMaybes $ forM solvingGroup $ \clsName -> do
              mCls <- getClass clsName
              opt  <- isOptionalClass clsName
              -- Optional classes that are not available, can be ignored while solving
              return $ if opt && isNothing mCls then
                         Nothing
                       else 
                         Just (clsName, mCls)
            -- If we found all of the classes in the solving group 
            -- (except the optional ones), we can try to solve the constraints.
            if all (isJust . snd) mClss then do 
              wantedCts <- getWantedConstraints
              solveConstraints (fmap (fromJust . snd) mClss) wantedCts
            -- We could not find all of the classes in the solving group: 
            -- Throw an error listing the missing classes. 
            else do
              throwPluginErrorSDoc $ O.hang (O.text "Missing classes:") errIndent 
                                   $ O.hcat $ O.punctuate (O.text ", ") 
                                   $ fmap (O.quotes . O.text . fst) 
                                 $ filter (isNothing . snd) mClss
    
    -- | Initialize the plugin environment.
    initSupermonadPlugin :: SupermonadPluginM () (ClassDict, InstanceDict)
    initSupermonadPlugin = do
      -- Determine if the supermonad module is available.
      eSupermonadMdl <- runTcPlugin $ findModuleByQuery mdlQuery
      _supermonadMdl <- case eSupermonadMdl of
        Right smMdl -> return smMdl
        Left mdlErrMsg -> throwPluginErrorSDoc mdlErrMsg
      
      -- Find the supermonad classes and instances and add the to the class dictionary.
      oldClsDict <- getClassDictionary
      newClsDict <- foldrM findClassesAndInstancesInScope oldClsDict clsQuery
      
      -- Calculate the supermonads in scope and check for rogue bind and return instances.
      let smInsts = findMonoTopTyConInstances newClsDict
      let smErrors = fmap snd $ checkInstances newClsDict smInsts (instImps newClsDict)
      
      -- Try to construct the environment or throw errors
      case smErrors of
        [] -> return (newClsDict, smInsts)
        _ -> do
          throwPluginErrorSDoc $ hang (text "Problems when finding supermonad instances:") errIndent $ vcat smErrors




