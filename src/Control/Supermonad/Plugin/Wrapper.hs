
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Provides version safe wrappers around GHC functions.
module Control.Supermonad.Plugin.Wrapper 
  ( -- * Type Variable Substitutions
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
    -- * Instance Environment
  , lookupInstEnv
  ) where

import qualified Type as T
import qualified Kind as K
import qualified Module as M
import qualified Coercion as C
import qualified InstEnv as IE
import qualified Class
import qualified RdrName as RdrN
import qualified TcType as TcT
import qualified TcRnTypes as TcRnT
import qualified TcEvidence as TcEv
import qualified SrcLoc


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


















