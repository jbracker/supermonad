
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Provides version safe wrappers around GHC functions.
module Control.Supermonad.Plugin.Wrapper 
  ( -- * General Utilities (To avoid import loop)
    fromLeft, fromRight
  , -- * Type Variable Substitutions
    TypeVarSubst
  , mkTypeVarSubst
    -- * Types
  , splitKindFunTys
  , mkEqualityCtType
  , constraintSourceLocation
    -- * Modules
  , UnitId
  , baseUnitId
  , moduleUnitId
  , isImportedFrom
    -- * Evidence Production
  , mkTcCoercion
  , produceTupleEvidence, isTupleTyCon
    -- * Instance Environment
  , lookupInstEnv
  ) where

import Data.Either ( isLeft )

import Control.Monad ( mapM )

import qualified Type as T
import qualified TyCon as TC
import qualified Kind as K
import qualified Module as M
import qualified Coercion as C
import qualified Outputable as O
import qualified InstEnv as IE
import qualified Class
import qualified RdrName as RdrN
import qualified TcType as TcT
import qualified TcRnTypes as TcRnT
import qualified TcEvidence as TcEv
import qualified TcPluginM
import qualified SrcLoc

-- | Return the 'Left' value. If no 'Left' value is given, an error is raised.
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft: Applied to 'Right'"

-- | Return the 'Right' value. If no 'Right' value is given, an error is raised.
fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight: Applied to 'Left'"
fromRight (Right b) = b

-- | Type of type variable substitutions.
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- 8.0.0+
type TypeVarSubst = T.TCvSubst
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
-- >7.10.1 && <8.0.0
type TypeVarSubst = T.TvSubst
#endif

-- | Make a substitution based on an association between type variables and types.
mkTypeVarSubst :: [(T.TyVar, T.Type)] -> TypeVarSubst
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
mkTypeVarSubst = T.mkTvSubstPrs
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
mkTypeVarSubst = T.mkTopTvSubst
#endif

-- | Splits a function kind into its argument kinds and its result.
--   
--   For example: @splitKindFunTys (* -> * -> Constraint) = ([*, *], Constraint)@
splitKindFunTys :: K.Kind -> ([K.Kind], K.Kind)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
splitKindFunTys = T.splitFunTys
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
splitKindFunTys = K.splitKindFunTys
#endif

-- | Create a type representing an equality constraint between the two arguments.
--   
--   For example: @mkEqualityCtType a Int = a ~ Int@
mkEqualityCtType :: T.Type -> T.Type -> T.Type
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
mkEqualityCtType = T.mkPrimEqPred -- Maybe we should use 'mkHeteroPrimEqPred' instead?
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
mkEqualityCtType = TcT.mkTcEqPred
#endif

-- | Returns the source code location of the given constraint.
constraintSourceLocation :: TcRnT.Ct -> SrcLoc.SrcSpan
#if MIN_VERSION_GLASGOW_HASKELL(7,10,2,0)
constraintSourceLocation = SrcLoc.RealSrcSpan . TcRnT.tcl_loc . TcRnT.ctl_env . TcRnT.ctev_loc . TcRnT.cc_ev
#else
constraintSourceLocation = TcRnT.tcl_loc . TcRnT.ctl_env . TcRnT.ctev_loc . TcRnT.cc_ev
#endif

-- | Type of package identifiers.
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- 8.0.0+
type UnitId = M.UnitId
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
-- >7.10.1 && <8.0.0
type UnitId = M.PackageKey
#endif

-- | The package identifier for the @base@ package.
baseUnitId :: UnitId
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
baseUnitId = M.baseUnitId
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
baseUnitId = M.basePackageKey
#endif

-- | Access the package identifier of the given module.
moduleUnitId :: M.Module -> UnitId
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
moduleUnitId = M.moduleUnitId
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
moduleUnitId = M.modulePackageKey
#endif

-- | Check if the given element is imported from the given module.
--   'False' if it is a local definition.
isImportedFrom :: RdrN.GlobalRdrElt -> M.Module -> Bool
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
isImportedFrom rdrElt mdl = if not (RdrN.gre_lcl rdrElt) 
  then any (M.moduleName mdl ==) (RdrN.importSpecModule <$> RdrN.gre_imp rdrElt) 
  else False
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
isImportedFrom rdrElt mdl = case RdrN.gre_prov rdrElt of
  RdrN.LocalDef -> False
  RdrN.Imported [] -> False
  RdrN.Imported impSpecs -> M.moduleName mdl == RdrN.importSpecModule (last impSpecs)
#endif

-- | Wrap a 'C.Coercion' into a 'TcEv.TcCoercion'.
mkTcCoercion :: C.Coercion -> TcEv.TcCoercion
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
mkTcCoercion = id
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
mkTcCoercion = TcEv.TcCoercion
#endif

-- | Evidence production step for "Control.Supermonad.Plugin.Evidence" module to produce
--   evidence for tuple constraints (prior to GHC 8.0.1).
produceTupleEvidence :: T.Type
                     -> TC.TyCon -> [T.Type] 
                     -> (T.Type -> TcPluginM.TcPluginM (Either O.SDoc TcEv.EvTerm))
                     -> TcPluginM.TcPluginM (Either O.SDoc TcEv.EvTerm)
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- From GHC 8.0.1 onward there is no necessity to solve constraint 
-- tuples in a special way anymore, because:
-- https://git.haskell.org/ghc.git/commitdiff/ffc21506894c7887d3620423aaf86bc6113a1071
produceTupleEvidence ct _tc _tcArgs _cont = return $ Left $ 
  O.text "Production of tuple evidence not necessary anymore! How did we get here?"
  O.$$ O.ppr ct
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
produceTupleEvidence ct _tc tcArgs cont = do
  -- Produce evidence for each element of the tuple
  tupleEvs <- mapM cont tcArgs
  return $ if any isLeft tupleEvs
    then Left
      $ O.text "Can't find evidence for this tuple constraint:"
      O.$$ O.ppr ct
      O.$$ O.text "Reason:"
      O.$$ O.vcat (fromLeft <$> filter isLeft tupleEvs)
    -- And put together evidence for the complete tuple.
    else Right $ TcEv.EvTupleMk $ fmap fromRight tupleEvs
#endif

-- | Check if a given type constructor is a tuple constructor.
isTupleTyCon :: TC.TyCon -> Bool
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
isTupleTyCon _tc = False
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
isTupleTyCon = TC.isTupleTyCon
#endif

-- | Lookup an instance for the class applied to the type arguments within the 
--   instance environment
lookupInstEnv :: IE.InstEnvs -> Class.Class -> [T.Type] -> IE.ClsInstLookupResult
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
lookupInstEnv instEnv cls tys = IE.lookupInstEnv False instEnv cls tys 
-- TODO / FIXME: Not really sure if we need to check the safe Haskell overlap restrictions 
-- or not. For now we don't.
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
lookupInstEnv instEnv cls tys = IE.lookupInstEnv instEnv cls tys
#endif


















