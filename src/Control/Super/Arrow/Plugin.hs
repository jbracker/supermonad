
-- This is only needed to generate Haddock documentation.
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the supermonad plugin for GHC.
module Control.Super.Arrow.Plugin
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
  , superarrowModuleName
  , legacySuperarrowModuleName
  , arrowArrClassName
  , arrowSequenceClassName
  , arrowSelectClassName
  , arrowCombineClassName )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

-- | The supermonad type checker plugin for GHC.
plugin :: Plugin
plugin = pluginPrototype moduleQuery [classQuery] solvingGroups instanceImplications

-- -----------------------------------------------------------------------------
-- Superarrow specific initialization
-- -----------------------------------------------------------------------------

-- | Configure which groups of classes need to be solved together.
solvingGroups :: [[PluginClassName]]
solvingGroups = 
  [ [ arrowArrClassName, arrowSequenceClassName, arrowSelectClassName, arrowCombineClassName ] ]

-- | Queries the module providing the superarrow classes.
moduleQuery :: ModuleQuery
moduleQuery = EitherModule
  [ AnyModule [ ThisModule superarrowModuleName Nothing
              , ThisModule legacySuperarrowModuleName Nothing
              ]
  ]{-, AnyModule [ ThisModule supermonadCtModuleName Nothing
              , ThisModule supermonadCtPreludeModuleName Nothing
              , ThisModule legacySupermonadCtModuleName Nothing
              , ThisModule legacySupermonadCtPreludeModuleName Nothing
              ]
  ]-} $ Just findSuperarrowModulesErrMsg

-- | Queries the superarrow classes.
classQuery :: ClassQuery
classQuery = ClassQuery moduleQuery 
  [ (arrowArrClassName     , 1)
  , (arrowSequenceClassName, 3)
  , (arrowSelectClassName  , 2)
  , (arrowCombineClassName , 3)
  ]

-- | Ensures that all supermonad instance implications with the group of 
--   one type constructor are obeyed.
instanceImplications :: ClassDict -> [InstanceImplication]
instanceImplications clsDict =
    (arrowArrClassName <=> arrowSequenceClassName) ++
    (arrowArrClassName <=> arrowSelectClassName  ) ++
    (arrowArrClassName <=> arrowCombineClassName )
  where
    (==>) = clsDictInstImp clsDict
    (<=>) = clsDictInstEquiv clsDict

-- | Function to produce proper error messages in the module query.
findSuperarrowModulesErrMsg :: [Either SDoc Module] -> SDoc
findSuperarrowModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find superarrow or constrained superarrow modules!") errIndent (errA $$ errB)
findSuperarrowModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained superarrow modules!"
findSuperarrowModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls


