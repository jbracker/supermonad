
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
  ( PluginClassName, PluginModuleName
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
plugin = pluginPrototype [ supermonadClassQuery, monadPlusClassQuery, alternativeClassQuery ]
                         solvingGroups
                         supermonadInstanceImplications

-- -----------------------------------------------------------------------------
-- Supermonad specific initialization
-- -----------------------------------------------------------------------------

alternativeEmptyClassName, alternativeAltClassName :: PluginClassName
alternativeEmptyClassName = "AlternativeEmpty"
alternativeAltClassName   = "AlternativeAlt"

alternativeModuleName, alternativeCtModuleName :: PluginModuleName
alternativeModuleName   = "Control.Super.Monad.Alternative"
alternativeCtModuleName = "Control.Super.Monad.Constrained.Alternative"

monadPlusZeroClassName, monadPlusAddClassName :: PluginClassName
monadPlusZeroClassName = "MonadPlusZero"
monadPlusAddClassName  = "MonadPlusAdd"

monadPlusModuleName, monadPlusCtModuleName :: PluginModuleName
monadPlusModuleName   = "Control.Super.Monad.MonadPlus"
monadPlusCtModuleName = "Control.Super.Monad.Constrained.MonadPlus"

-- | Configure which groups of classes need to be solved together.
solvingGroups :: [[PluginClassName]]
solvingGroups = 
  [ [ bindClassName, returnClassName, applicativeClassName ]
    -- , alternativeEmptyClassName, alternativeAltClassName ] -- Supermonad group
  ]

-- | Queries the module providing the supermonad classes.
supermonadModuleQuery :: ModuleQuery
supermonadModuleQuery = EitherModule
  [ AnyModule [ ThisModule supermonadModuleName              Nothing
              , ThisModule supermonadPreludeModuleName       Nothing
              , ThisModule legacySupermonadModuleName        Nothing
              , ThisModule legacySupermonadPreludeModuleName Nothing
              ]
  , AnyModule [ ThisModule supermonadCtModuleName              Nothing
              , ThisModule supermonadCtPreludeModuleName       Nothing
              , ThisModule legacySupermonadCtModuleName        Nothing
              , ThisModule legacySupermonadCtPreludeModuleName Nothing
              ]
  ] $ Just findSupermonadModulesErrMsg

alternativeModuleQuery :: ModuleQuery
alternativeModuleQuery = EitherModule
  [ ThisModule alternativeModuleName   Nothing
  , ThisModule alternativeCtModuleName Nothing
  ] $ Just findAlternativeModulesErrMsg

monadPlusModuleQuery :: ModuleQuery
monadPlusModuleQuery = EitherModule
  [ ThisModule monadPlusModuleName   Nothing
  , ThisModule monadPlusCtModuleName Nothing
  ] $ Just findMonadPlusModulesErrMsg

-- | Queries the supermonad classes.
supermonadClassQuery :: ClassQuery
supermonadClassQuery = ClassQuery False supermonadModuleQuery 
  [ (bindClassName       , 3)
  , (returnClassName     , 1)
  , (applicativeClassName, 3)
  ]

alternativeClassQuery :: ClassQuery
alternativeClassQuery = ClassQuery True alternativeModuleQuery
  [ (alternativeAltClassName  , 3)
  , (alternativeEmptyClassName, 1)
  ]

monadPlusClassQuery :: ClassQuery
monadPlusClassQuery = ClassQuery True monadPlusModuleQuery
  [ (monadPlusZeroClassName, 1)
  , (monadPlusAddClassName , 3)
  ]

-- | Ensures that all supermonad instance implications with the group of 
--   one type constructor are obeyed.
supermonadInstanceImplications :: ClassDict -> [InstanceImplication]
supermonadInstanceImplications clsDict =
    (applicativeClassName      ==> returnClassName        ) ++
    (bindClassName             ==> returnClassName        ) ++
    (alternativeEmptyClassName <=> alternativeAltClassName) ++
    (monadPlusZeroClassName    <=> monadPlusAddClassName  )
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

-- | Function to produce proper error messages in the module query.
findAlternativeModulesErrMsg :: [Either SDoc Module] -> SDoc
findAlternativeModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find alternative or constrained alternative modules!") errIndent (errA $$ errB)
findAlternativeModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained alternative modules!"
findAlternativeModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls

-- | Function to produce proper error messages in the module query.
findMonadPlusModulesErrMsg :: [Either SDoc Module] -> SDoc
findMonadPlusModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find monad plus or constrained monad plus modules!") errIndent (errA $$ errB)
findMonadPlusModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained monad plus modules!"
findMonadPlusModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls