
{-# LANGUAGE CPP #-}

-- | Functions and utilities to work with and inspect constraints
--   of the GHC API.
module Control.Supermonad.Plugin.Constraint
  ( -- * Types
    GivenCt, WantedCt, DerivedCt
    -- * Constraint Creation
  , mkDerivedTypeEqCt
  , mkDerivedTypeEqCt'
  , mkDerivedClassCt
    -- * Constraint inspection
  , isClassConstraint
  , constraintClassType
  , constraintClassTyArgs
  , constraintClassTyCon
  , constraintTyCons
  , constraintTcVars
  , constraintLocation
  , constraintSourceLocation
  , sortConstraintsByLine
  , constraintTyVars
  ) where

import Data.List ( sortBy )
import Data.Set ( Set )
import qualified Data.Set as S

import SrcLoc ( SrcSpan(..) )
import TcRnTypes
  ( Ct(..), CtLoc(..), CtEvidence(..)
  , TcLclEnv( tcl_loc )
  , mkNonCanonical )
import Class ( Class(..) )
import Type
  ( Type, TyVar
  , mkTyVarTy, mkAppTys, mkTyConTy
  , getClassPredTys_maybe
  )
import TyCon ( TyCon )
import TcType ( mkTcEqPred )

import Control.Supermonad.Plugin.Utils
  ( collectTopTyCons
  , collectTopTcVars
  , collectTyVars )

-- | Type synonym to label given or derived constraints.
type GivenCt = Ct

-- | Type synonym to label derived constraints.
type DerivedCt = Ct

-- | Type synonym to label wanted constraints.
type WantedCt = Ct

-- -----------------------------------------------------------------------------
-- Constraint Creation
-- -----------------------------------------------------------------------------

-- | Create a derived type equality constraint. The constraint
--   will be located at the location of the given constraints
--   and equate the given variable with the given type.
mkDerivedTypeEqCt :: Ct -> TyVar -> Type -> Ct
mkDerivedTypeEqCt ct = mkDerivedTypeEqCt' (constraintLocation ct)

-- | Create a derived type equality constraint. The constraint
--   will be located at the given location
--   and equate the given variable with the given type.
mkDerivedTypeEqCt' :: CtLoc -> TyVar -> Type -> Ct
mkDerivedTypeEqCt' loc tv ty = mkNonCanonical CtDerived
  { ctev_pred = mkTcEqPred (mkTyVarTy tv) ty
  , ctev_loc = loc }

-- | Creates a derived class constraint using the given location
--   as origin. It is the programmers responsibility to supply the
--   correct number of type arguments for the given class.
mkDerivedClassCt :: CtLoc -> Class -> [Type] -> Ct
mkDerivedClassCt loc cls ts = mkNonCanonical CtDerived
  { ctev_pred = mkAppTys (mkTyConTy $ classTyCon cls) ts
  , ctev_loc = loc }

-- -----------------------------------------------------------------------------
-- Constraint Inspection
-- -----------------------------------------------------------------------------

-- | Check if the given constraint is a class constraint of the given class.
isClassConstraint :: Class -> Ct -> Bool
isClassConstraint wantedClass ct =
  case ct of
    CDictCan { cc_class = cls } -> cls == wantedClass
    CNonCanonical { cc_ev = ev } -> case getClassPredTys_maybe (ctev_pred ev) of
      Just (cls, _args) -> cls == wantedClass
      _ -> False
    _ -> False

-- | Retrieves the class and type arguments of the given
--   type class constraint.
--   Only works if the constraint is a type class constraint, otherwise
--   returns 'Nothing'.
constraintClassType :: Ct -> Maybe (Class, [Type])
constraintClassType ct = case ct of
  CDictCan {} -> Just (cc_class ct, cc_tyargs ct)
  CNonCanonical evdnc -> getClassPredTys_maybe $ ctev_pred evdnc
  _ -> Nothing

-- | Retrieves the arguments of the given constraints.
--   Only works if the given constraint is a type class constraint.
--   See 'constraintClassType'.
constraintClassTyArgs :: Ct -> Maybe [Type]
constraintClassTyArgs = fmap snd . constraintClassType

-- | Retrieves the type constructor of the given type class constraint.
--   See 'constraintClassType'.
constraintClassTyCon :: Ct -> Maybe TyCon
constraintClassTyCon = fmap (classTyCon . fst) . constraintClassType

-- | Collects the type constructors in the arguments of the constraint.
--   Only works if the given constraint is a type class constraint.
--   Only collects those on the top level (See 'collectTopTyCons').
constraintTyCons :: Ct -> Set TyCon
constraintTyCons ct = maybe S.empty collectTopTyCons $ constraintClassTyArgs ct

-- | Collects the type variables in the arguments of the constraint.
--   Only works if the given constraint is a type class constraint.
--   Only collects those on the top level (See 'collectTopTcVars').
constraintTcVars :: Ct -> Set TyVar
constraintTcVars ct = maybe S.empty collectTopTcVars $ constraintClassTyArgs ct

-- | Retrieve the source location the given constraint originated from.
constraintLocation :: Ct -> CtLoc
constraintLocation ct = ctev_loc $ cc_ev ct

-- | Returns the source code location of the given constraint.
constraintSourceLocation :: Ct -> SrcSpan
#if MIN_VERSION_ghc(7,10,2)
constraintSourceLocation = RealSrcSpan . tcl_loc . ctl_env . constraintLocation
#else
constraintSourceLocation = tcl_loc . ctl_env . constraintLocation
#endif

constraintTyVars :: Ct -> Set TyVar
constraintTyVars = collectTyVars . ctev_pred . cc_ev

-- | Sorts constraints by the line of their occurence.
sortConstraintsByLine :: [Ct] -> [Ct]
sortConstraintsByLine = sortBy cmpLine
  where
    cmpLine :: Ct -> Ct -> Ordering
    cmpLine ct1 ct2 = compare (constraintSourceLocation ct1) (constraintSourceLocation ct2)