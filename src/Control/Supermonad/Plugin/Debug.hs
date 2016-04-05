
module Control.Supermonad.Plugin.Debug
  ( containsAnyOf
  , containsAllOf
  , containsNoneOf
  , pprToStr, sDocToStr
  , hasEvTermPattern
  ) where

import Data.List ( isInfixOf )

import Outputable
  ( Outputable(..), SDoc
  , showSDocUnsafe )
import Coercion ( Coercion(..) )
import TcEvidence 
  ( TcCoercion(..)
  , EvTerm(..), EvTypeable(..), EvCallStack(..) )

-- | Convert some generic outputable to a string (Unsafe).
pprToStr :: Outputable o => o -> String
pprToStr = sDocToStr . ppr

sDocToStr :: SDoc -> String
sDocToStr = showSDocUnsafe

containsAnyOf :: (Outputable o) => o -> [String] -> Bool
containsAnyOf obj = any (`isInfixOf` pprToStr obj)

containsAllOf :: (Outputable o) => o -> [String] -> Bool
containsAllOf obj = all (`isInfixOf` pprToStr obj)

containsNoneOf :: (Outputable o) => o -> [String] -> Bool
containsNoneOf obj = not . (obj `containsAnyOf`)


hasCoercionPattern :: Coercion -> Bool
hasCoercionPattern coer = case coer of
  (Refl _r _t) -> False
  (TyConAppCo _r _tc cs) -> any hasCoercionPattern cs
  (AppCo ca cb) -> hasCoercionPattern ca || hasCoercionPattern cb
  (ForAllCo _tv c) -> hasCoercionPattern c
  (CoVarCo _cv) -> False
  (AxiomInstCo _ca _bi cs) -> any hasCoercionPattern cs
  (UnivCo _fs _r _ta _tb) -> False
  (SymCo c) -> hasCoercionPattern c
  (TransCo ca cb) -> hasCoercionPattern ca || hasCoercionPattern cb
  (AxiomRuleCo _car _ts cs) -> any hasCoercionPattern cs
  (LRCo _lor c) -> hasCoercionPattern c
  (InstCo c _t) -> hasCoercionPattern c
  (SubCo c) -> hasCoercionPattern c
  (NthCo 0 (NthCo 2 _c)) -> True
  (NthCo _i c) -> hasCoercionPattern c

hasTcCoercionPattern :: TcCoercion -> Bool
hasTcCoercionPattern coer = case coer of
  (TcRefl _r _t) -> False
  (TcTyConAppCo _r _tc tcs) -> any hasTcCoercionPattern tcs
  (TcAppCo tca tcb) -> hasTcCoercionPattern tca || hasTcCoercionPattern tcb
  (TcForAllCo _tv tc) -> hasTcCoercionPattern tc
  (TcCoVarCo _ev) -> False
  (TcAxiomInstCo _cab _i tcs) -> any hasTcCoercionPattern tcs
  (TcAxiomRuleCo _car _ts tcs) -> any hasTcCoercionPattern tcs
  (TcPhantomCo _ta _tb) -> False
  (TcSymCo tc) -> hasTcCoercionPattern tc
  (TcTransCo tca tcb) -> hasTcCoercionPattern tca || hasTcCoercionPattern tcb
  (TcLRCo _lor tc) -> hasTcCoercionPattern tc
  (TcSubCo tc) -> hasTcCoercionPattern tc
  (TcCastCo tca tcb) -> hasTcCoercionPattern tca || hasTcCoercionPattern tcb
  (TcLetCo _eb tc) -> hasTcCoercionPattern tc
  (TcCoercion c) -> hasCoercionPattern c
  (TcNthCo 0 (TcNthCo 2 _c)) -> True
  (TcNthCo _t tc) -> hasTcCoercionPattern tc

hasEvTermPattern :: EvTerm -> Bool
hasEvTermPattern term = case term of
  (EvId _id) -> False
  (EvCoercion tc) -> hasTcCoercionPattern tc
  (EvCast et tc) -> hasTcCoercionPattern tc || hasEvTermPattern et
  (EvDFunApp _dfid _ts ets) -> any hasEvTermPattern ets
  (EvTupleSel et _i) -> hasEvTermPattern et
  (EvTupleMk ets) -> any hasEvTermPattern ets
  (EvDelayedError _t _fs) -> False
  (EvSuperClass et _i) -> hasEvTermPattern et
  (EvLit _lit) -> False
  (EvTypeable evty) -> hasEvTypeablePattern evty
  (EvCallStack evcs) -> hasEvCallStackPattern evcs

hasEvTypeablePattern :: EvTypeable -> Bool
hasEvTypeablePattern evTy = case evTy of
  (EvTypeableTyCon _tc _ks) -> False
  (EvTypeableTyApp (eta, _ta) (etb, _tb)) -> hasEvTermPattern eta || hasEvTermPattern etb
  (EvTypeableTyLit _t) -> False

hasEvCallStackPattern :: EvCallStack -> Bool
hasEvCallStackPattern evcs = case evcs of
  (EvCsEmpty) -> False
  (EvCsPushCall _n _rss et) -> hasEvTermPattern et
  (EvCsTop _fs _rss et) -> hasEvTermPattern et
  