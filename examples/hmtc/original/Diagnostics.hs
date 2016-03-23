{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Diagnostics                                          *
*       Purpose:        Diagnostic messages and computations (monad)         *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Diagnostic messages and computations (monad). The module provides
-- a datatype for representing diagnostic messages, tagged with
-- source-code position and severity level (error, warning, information).
-- It also provides an abstraction for diagnostic computations, where
-- a computation has the ability to emit diagnostic messages and, possibly,
-- fail. Finally, it provides a utility function for reporting internal
-- compiler errors in a standardized way.

-- Refactored 2013 to support monadic fixpoints (in a robust manner)
-- Updated 2015 in view of revised monad class hierarchy.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Diagnostics (
    -- * Diagnostic messages
    DMsg (..),          -- Not abstract. Instances: HasSrcPos.
    DMLvl (..),         -- Not abstract. Instances: Eq, Ord.
    mkInfoMsg,          -- :: SrcPos -> String -> DMsg
    mkWngMsg,           -- :: SrcPos -> String -> DMsg
    mkErrMsg,           -- :: SrcPos -> String -> DMsg
    dmIsInfo,           -- :: DMsg -> Bool
    dmIsWng,            -- :: DMsg -> Bool
    dmIsErr,            -- :: DMsg -> Bool
    dmCmpLvlPos,        -- :: DMsg -> DMsg -> Ordering
    ppDMsg,             -- :: DMsg -> String

    -- * Diagnostic computations
    Diagnostic(..),     -- Class for diagnostic computation
    DiagnosticFail(..), -- Class for diagnostic computations with failure
    D,                  -- Abstract. Instances: Monad, MonadFix, Diagnostic
    runD,               -- :: D a -> (a, [DMsg])
    DF,                 -- Abstract. Inst.: Monad, Diagnostic, DiagnosticFail
    dToDF,              -- :: D a -> DF a
    runDF,              -- :: D a -> (Maybe a, [DMsg])

    -- * Reporting and checking for internal errors
    internalError,      -- :: String -> String -> String -> a
    assert,             -- :: Bool -> String -> String -> String -> a -> a
    assertM             -- :: Monad m =>
                        --        Bool -> String -> String -> String -> m ()
) where

-- Standard library imports
import Data.List (sortBy)
import Control.Monad.Fix
import Control.Applicative      -- Backwards compatibibility

-- HMTC module imports
import SrcPos

infixr 9 |||


------------------------------------------------------------------------------
-- Diagnostic messages
------------------------------------------------------------------------------

-- | Representation of diagnostic messages
data DMsg = DMsg {
         dmLvl    :: DMLvl,             -- ^ Severity level
         dmSrcPos :: SrcPos,            -- ^ Associated source position
         dmTxt    :: String             -- ^ Message text
     }


-- | Severity levels
data DMLvl = DMLInfo    -- ^ Information (least severe)
           | DMLWng     -- ^ Warning
           | DMLErr     -- ^ Error (most severe)
           deriving (Eq, Ord)


instance HasSrcPos DMsg where
    srcPos = dmSrcPos


-- | Constructs information message.
mkInfoMsg :: SrcPos -> String -> DMsg
mkInfoMsg sp msg = DMsg {
                       dmLvl    = DMLInfo,
                       dmSrcPos = sp,
                       dmTxt    = msg
                   }

-- | Constructs warning message.
mkWngMsg :: SrcPos -> String -> DMsg
mkWngMsg sp msg = DMsg {
                      dmLvl    = DMLWng,
                      dmSrcPos = sp,
                      dmTxt    = msg
                  }


-- | Constructs error message.
mkErrMsg :: SrcPos -> String -> DMsg
mkErrMsg sp msg = DMsg {
                      dmLvl    = DMLErr,
                      dmSrcPos = sp,
                      dmTxt    = msg
                  }


-- | Checks if information message.
dmIsInfo :: DMsg -> Bool
dmIsInfo (DMsg {dmLvl = DMLInfo}) = True
dmIsInfo _                        = False


-- | Checks if warning message.
dmIsWng :: DMsg -> Bool
dmIsWng (DMsg {dmLvl = DMLWng}) = True
dmIsWng _                       = False


-- | Checks if error message.
dmIsErr :: DMsg -> Bool
dmIsErr (DMsg {dmLvl = DMLErr}) = True
dmIsErr _                       = False


-- | Comparison function for ordering messages first by severity, then
-- by source position.
dmCmpLvlPos :: DMsg -> DMsg -> Ordering
dmCmpLvlPos (DMsg {dmLvl = lvl1, dmSrcPos = sp1})
            (DMsg {dmLvl = lvl2, dmSrcPos = sp2}) =
    case compare lvl1 lvl2 of
        LT -> GT        -- Unsevere messages last
        GT -> LT        -- Severe messages first
        EQ -> compare sp1 sp2


-- | Formats a diagnostic message for printing.
ppDMsg :: DMsg -> String
ppDMsg (DMsg {dmLvl = lvl, dmSrcPos = sp, dmTxt = msg}) =
    kind ++ srcPosTxt sp ++ ":\n" ++ msg ++ "\n"
    where
        kind = case lvl of
                   DMLInfo -> "Information"
                   DMLWng  -> "Warning"
                   DMLErr  -> "Error"

        srcPosTxt NoSrcPos = ""
        srcPosTxt sp       = " at " ++ show sp


------------------------------------------------------------------------------
-- Monad transformer and Diagnostic computation classes
------------------------------------------------------------------------------

-- | Monad transformer

-- This version requires LANGUAGE MultiParamTypeClasses, FlexibleInstances.
-- But want to avoid enabling such extensions for just this one small thing.
--
-- class (Monad m, Monad (t m)) => MonadTransformer t m where
--     lift :: m a -> t m a
--
-- The FlexibleInstances extension is needed for an instantiation like:
--
-- instance Monad m => MonadTransformer DFT m where
--     lift m = DFT $ m >>= \a -> return (Just a)

class MonadTransformer t where
    lift :: Monad m => m a -> t m a


-- | Class for diagnostic computations. Diagnostic computations accumulate
-- diagnostic messages.
-- Context "Applicative d" for backwards compatibility
class (Applicative d, Monad d) => Diagnostic d where

    -- | Emits a diagnostic message
    emitD :: DMsg -> d ()

    -- | Emits an information message.
    emitInfoD :: SrcPos -> String -> d ()
    emitInfoD sp msg = emitD (mkInfoMsg sp msg)

    -- | Emits a warning message.
    emitWngD :: SrcPos -> String -> d ()
    emitWngD sp msg = emitD (mkWngMsg sp msg)

    -- | Emits an error message.
    emitErrD :: SrcPos -> String -> d ()
    emitErrD sp msg = emitD (mkErrMsg sp msg)

    -- | Diagnostic messages emitted thus far
    getDMsgsD :: d [DMsg]

    -- | Tries the first diagnostic computation. If that results in errors,
    -- discards those and runs the second computation.
    (|||) :: d a -> d a -> d a


-- | Class for diagnostic computations with failure.
class Diagnostic df => DiagnosticFail df where

    -- | Emits an error message and fails.
    failD :: SrcPos -> String -> df a
    
    -- | Fails without giving any specific reason.
    failNoReasonD :: df a
    
    -- | Fails if there has been errors thus far
    failIfErrorsD :: df ()
    
    -- | Forces a stop, e.g. after some user-specified pass.
    stopD :: df a


------------------------------------------------------------------------------
-- Diagnostic computations
------------------------------------------------------------------------------

-- Note on the use of '$':
-- '$' is the explicit function application operator in Haskell.
-- It is useful because it binds less tightly than normal function
-- application. This allows parentheses around (potentially large)
-- function arguments to be omitted. For example, below, instead
-- of writing
--
--     D (\dms -> ...)
--
-- I write
--
--     D $ \dms -> ...
--
-- This also allows the "..." to be spread out over subsequent lines
-- without worrying about the final ")".


-- | Diagnostic computation.

newtype D a = D ([DMsg] -> (a, [DMsg]))


unD :: D a -> ([DMsg] -> (a, [DMsg]))
unD (D x) = x


instance Functor D where
    fmap f d = D $ \dms ->
        let
            (a, dms') = unD d dms
        in
            (f a, dms')

    a <$ d = D $ \dms ->
        let
            (_, dms') = unD d dms
        in
            (a, dms')


instance Applicative D where
    pure a = D $ \dms -> (a, dms)

    df <*> da = D $ \dms ->
        let
            (f, dms')  = unD df dms
            (a, dms'') = unD da dms'
        in
            (f a, dms'')


instance Monad D where
    return = pure               -- Backwards compatibility

    d >>= f = D $ \dms ->
        let
            (a, dms') = unD d dms
        in
            unD (f a) dms'


instance MonadFix D where
    mfix f = D $ \dms ->
        let
            adms'@(a, _) = unD (f a) dms
        in
            adms'


instance Diagnostic D where
    emitD dm = D $ \dms -> ((), dm : dms)

    getDMsgsD = D $ \dms -> (dms, dms)

    d1 ||| d2 = D $ \dms ->
        let
            (a1, dms1) = unD d1 []
        in
            if any dmIsErr dms1 then
                unD d2 dms
            else
                (a1, dms1 ++ dms)


-- | Runs a diagnostic computation. Returns:
--
-- (1) Result of the computation.
--
-- (2) Sorted list of diagnostic messages.

runD :: D a -> (a, [DMsg])
runD d = (a, sortBy dmCmpLvlPos dms)
    where
        (a, dms) = unD d []


------------------------------------------------------------------------------
-- Transformer to diagnostic computation with failure
------------------------------------------------------------------------------

newtype DFT m a = DFT (m (Maybe a))


unDFT :: DFT m a -> m (Maybe a)
unDFT (DFT m) = m


instance Functor m => Functor (DFT m) where
    fmap f m = DFT $ fmap (fmap f) (unDFT m)

    a <$ m = DFT $ fmap (a <$) (unDFT m)        -- Just rely on default inst.?


instance Applicative m => Applicative (DFT m) where
    pure a = DFT $ pure (Just a)

    mf <*> ma = DFT $ fmap (<*>) (unDFT mf) <*> (unDFT ma)


-- Context "Applicative m" for backwards compatibility.
instance (Applicative m, Monad m) => Monad (DFT m) where
    return = pure       -- Backwards compatibility

    m >>= f = DFT $
        unDFT m >>= \ma ->
        case ma of
            Nothing -> return Nothing
            Just a  -> unDFT (f a)


instance MonadTransformer DFT where
    lift m = DFT $ m >>= \a -> return (Just a)


instance Diagnostic d => Diagnostic (DFT d) where
    emitD dm = lift (emitD dm)

    getDMsgsD = lift getDMsgsD

    dftd1 ||| dftd2 = DFT $ unDFT dftd1 ||| unDFT dftd2


instance Diagnostic d => DiagnosticFail (DFT d) where
    failD sp msg = DFT $ emitErrD sp msg >> return Nothing
    
    failNoReasonD = DFT $ emitErrD NoSrcPos "Failure, unknown reason"
                          >> return Nothing

    failIfErrorsD = DFT $
        getDMsgsD >>= \dms ->
        if any dmIsErr dms then
            return Nothing
        else
            return (Just ())
    
    stopD = DFT $ return Nothing


------------------------------------------------------------------------------
-- Diagnostic computation with failure
------------------------------------------------------------------------------

newtype DF a = DF (DFT D a) deriving (Functor, Applicative, Monad,
                                      Diagnostic, DiagnosticFail)


unDF :: DF a -> DFT D a
unDF (DF dfta) = dfta

dToDF :: D a -> DF a
dToDF = DF . lift


runDF :: DF a -> (Maybe a, [DMsg])
runDF = runD . unDFT . unDF


------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

-- | Signals an internal compiler error.
-- Call with module name, name of function, and error message
-- to report internal errors; i.e., things that should not happen.

internalError :: String -> String -> String -> a
internalError m f msg =
    error ("[Internal Error] " ++ m ++ "." ++ f ++ ": " ++ msg )


-- | Checks that a condition holds and returns given value if so.
-- Signals an internal compiler error otherwise.
-- Call with condition, module name, name of function, error message, and
-- value to return if condition satisfied.

assert :: Bool -> String -> String -> String -> a -> a
assert c m f msg v | c         = v
                   | otherwise = internalError m f msg
 

-- | Checks that a condition holds. Signals an internal compiler error
-- otherwise. Call with condition, module name, name of function, error
-- message.

assertM :: Monad m => Bool -> String -> String -> String -> m ()
assertM c m f msg = assert c m f msg (return ())
