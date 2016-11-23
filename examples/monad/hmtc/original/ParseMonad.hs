{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         ParseMonad                                           *
*       Purpose:        Monad for scanning and parsing                       *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Monad for scanning and parsing. 
-- The scanner and parser are both monadic, following the design outlined
-- in the Happy documentation on monadic parsers. The parse monad P
-- is built on top of the diagnostics monad D, additionally keeping track
-- of the input and current source code position, and exploiting that
-- the source code position is readily available to avoid having to pass
-- the position as an explicit argument.

-- Updated 2015 in view of revised monad class hierarchy.

module ParseMonad (
    -- The parse monad
    P (..),             -- Not abstract. Instances: Monad.
    unP,                -- :: P a -> (Int -> Int -> String -> DF a)
    emitInfoP,          -- :: String -> P ()
    emitWngP,           -- :: String -> P ()
    emitErrP,           -- :: String -> P ()
    failP,              -- :: String -> P a
    getSrcPosP,         -- :: P SrcPos
    runP                -- :: String -> P a -> DF a
) where

-- Standard library imports
import Control.Applicative      -- Backwards compatibibility


-- HMTC module imports
import SrcPos
import Diagnostics


newtype P a = P (Int -> Int -> String -> DF a)


unP :: P a -> (Int -> Int -> String -> DF a)
unP (P x) = x


instance Functor P where
    fmap f p = P (\l c s -> fmap f (unP p l c s))

    a <$ p   = P (\l c s -> a <$ (unP p l c s))


instance Applicative P where
    pure a = P (\_ _ _ -> pure a)

    pf <*> pa = P (\l c s -> unP pf l c s <*> unP pa l c s)


instance Monad P where
    return = pure               -- Backwards compatibility

    p >>= f = P (\l c s -> unP p l c s >>= \a -> unP (f a) l c s)


-- Liftings of useful computations from the underlying DF monad, taking
-- advantage of the fact that source code positions are available.

-- | Emits an information message.
emitInfoP :: String -> P ()
emitInfoP msg = P (\l c _ -> emitInfoD (SrcPos l c) msg)


-- | Emits a warning message.
emitWngP :: String -> P ()
emitWngP msg = P (\l c _ -> emitWngD (SrcPos l c) msg)


-- | Emits an error message.
emitErrP :: String -> P ()
emitErrP msg = P (\l c _ -> emitErrD (SrcPos l c) msg)


-- | Emits an error message and fails.
failP :: String -> P a
failP msg = P (\l c _ -> failD (SrcPos l c) msg)


-- | Gets the current source code position.
getSrcPosP :: P SrcPos
getSrcPosP = P (\l c _ -> return (SrcPos l c))


-- | Runs parser (and scanner), yielding a result in the diagnostics monad DF.
runP :: P a -> String -> DF a
runP p s = unP p 1 1 s
