
-- | Provides versions of functions written for 'TcPluginM'
--   that are lifted into 'SupermonadPluginM'.
module Control.Supermonad.Plugin.Environment.Lift
  (
  -- * From 'Control.Supermonad.Plugin.Evidence'
    produceEvidenceForCt
  , produceEvidenceFor
  , isPotentiallyInstantiatedCt
  -- * From 'Control.Supermonad.Plugin.Utils'
  , partiallyApplyTyCons
  -- * From 'Control.Supermonad.Plugin.Detect'
  , findClassesAndInstancesInScope
  ) where

import TcRnTypes ( Ct )
import TcEvidence ( EvTerm )
import Outputable ( SDoc )
import Type ( Type, TyVar )
import TyCon ( TyCon )
import InstEnv ( ClsInst )

import Control.Supermonad.Plugin.Environment
  ( SupermonadPluginM
  , runTcPlugin
  , getGivenConstraints
  , throwPluginErrorSDoc
  )
import Control.Supermonad.Plugin.ClassDict ( ClassDict, insertClsDict )

import qualified Control.Supermonad.Plugin.Utils as U
import qualified Control.Supermonad.Plugin.Detect as D
import qualified Control.Supermonad.Plugin.Evidence as E

-- | See 'E.produceEvidenceForCt'.
produceEvidenceForCt :: Ct -> SupermonadPluginM s (Either SDoc EvTerm)
produceEvidenceForCt ct = do
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceForCt givenCts ct

-- | See 'E.produceEvidenceFor'.
produceEvidenceFor :: ClsInst -> [Type] -> SupermonadPluginM s (Either SDoc EvTerm)
produceEvidenceFor inst instArgs = do
  givenCts <- getGivenConstraints
  runTcPlugin $ E.produceEvidenceFor givenCts inst instArgs

-- | See 'E.isPotentiallyInstantiatedCt'.
isPotentiallyInstantiatedCt :: Ct -> [(TyVar, Either TyCon TyVar)] -> SupermonadPluginM s Bool
isPotentiallyInstantiatedCt ct assoc = do
  givenCts <- getGivenConstraints
  runTcPlugin $ E.isPotentiallyInstantiatedCt givenCts ct assoc

-- | See 'U.partiallyApplyTyCons'.
partiallyApplyTyCons :: [(TyVar, Either TyCon TyVar)] -> SupermonadPluginM s (Either SDoc [(TyVar, Type, [TyVar])])
partiallyApplyTyCons = runTcPlugin . U.partiallyApplyTyCons

-- | See 'D.findClassesAndInstancesInScope'. In addition to calling the 
--   function from the @Detect@ module it also throws an error if the call
--   fails. Otherwise, inserts the found classes and instances into the provided 
--   class dictionary and returns the updated dictionary.
findClassesAndInstancesInScope :: D.ClassQuery -> ClassDict -> SupermonadPluginM s ClassDict
findClassesAndInstancesInScope clsQuery oldClsDict = do
  eFoundClsInsts <- runTcPlugin $ D.findClassesAndInstancesInScope clsQuery
  foundClsInsts <- case eFoundClsInsts of
    Right clsInsts -> return clsInsts
    Left errMsg -> throwPluginErrorSDoc errMsg
  return $ foldr (\(clsName, cls, clsInsts) -> insertClsDict clsName cls clsInsts) oldClsDict foundClsInsts