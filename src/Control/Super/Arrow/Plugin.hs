
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
  , clsDictInstEquiv -- , clsDictInstImp
  , defaultFindEitherModuleErrMsg )
import Control.Super.Plugin.Names
  ( PluginClassName
  , superarrowModuleName
  , superarrowCtModuleName
  , arrowArrClassName
  , arrowSequenceClassName
  , arrowSelectClassName
  , arrowParallelClassName
  , arrowFanOutClassName )

-- -----------------------------------------------------------------------------
-- The Plugin
-- -----------------------------------------------------------------------------

-- | The supermonad type checker plugin for GHC.
plugin :: Plugin
plugin = pluginPrototype [classQuery] solvingGroups instanceImplications

-- -----------------------------------------------------------------------------
-- Superarrow specific initialization
-- -----------------------------------------------------------------------------

-- | Configure which groups of classes need to be solved together.
solvingGroups :: [[PluginClassName]]
solvingGroups = 
  [ [ arrowArrClassName
    , arrowSequenceClassName, arrowSelectClassName
    , arrowParallelClassName, arrowFanOutClassName ] ]

-- | Queries the module providing the superarrow classes.
moduleQuery :: ModuleQuery
moduleQuery = EitherModule
  [ AnyModule [ ThisModule superarrowModuleName Nothing ]
  , AnyModule [ ThisModule superarrowCtModuleName Nothing ]
  ] $ Just findSuperarrowModulesErrMsg

-- | Queries the superarrow classes.
classQuery :: ClassQuery
classQuery = ClassQuery False moduleQuery 
  [ (arrowArrClassName     , 1)
  , (arrowSequenceClassName, 3)
  , (arrowSelectClassName  , 2)
  , (arrowParallelClassName, 3)
  , (arrowFanOutClassName  , 3)
  ]

-- | Ensures that all supermonad instance implications with the group of 
--   one type constructor are obeyed.
instanceImplications :: ClassDict -> [InstanceImplication]
instanceImplications clsDict =
    (arrowArrClassName <=> arrowSequenceClassName) ++
    (arrowArrClassName <=> arrowSelectClassName  ) ++
    (arrowArrClassName <=> arrowParallelClassName) ++
    (arrowArrClassName <=> arrowFanOutClassName  )
  where
    -- (==>) = clsDictInstImp clsDict
    (<=>) = clsDictInstEquiv clsDict

-- | Function to produce proper error messages in the module query.
findSuperarrowModulesErrMsg :: [Either SDoc Module] -> SDoc
findSuperarrowModulesErrMsg [Left errA, Left errB] = 
  hang (text "Could not find superarrow or constrained superarrow modules!") errIndent (errA $$ errB)
findSuperarrowModulesErrMsg [Right _mdlA, Right _mdlB] =
  text "Found unconstrained and constrained superarrow modules!"
findSuperarrowModulesErrMsg mdls = defaultFindEitherModuleErrMsg mdls


