
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Super.Monad.Plugin
  ( plugin ) where

import Plugins ( Plugin )
import Outputable ( SDoc, hang, text, ($$) )
import Module ( Module )

import Control.Super.Plugin.Prototype ( pluginPrototype )

import Control.Super.Plugin.Utils ( errIndent )
--import qualified Control.Super.Plugin.Log as L
import Control.Super.Plugin.ClassDict ( ClassDict )
import Control.Super.Plugin.Detect 
  ( ModuleQuery(..)
  , ClassQuery(..)
  , InstanceImplication
  , clsDictInstImp, clsDictInstEquiv
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
plugin = pluginPrototype supermonadModuleQuery
                         [supermonadClassQuery]
                         solvingGroups
                         supermonadInstanceImplications

-- -----------------------------------------------------------------------------
-- Supermonad specific initialization
-- -----------------------------------------------------------------------------

-- | Configure which groups of classes need to be solved together.
solvingGroups :: [[PluginClassName]]
solvingGroups = 
  [ [ bindClassName, returnClassName, applicativeClassName ] -- Supermonad group
  ]

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


