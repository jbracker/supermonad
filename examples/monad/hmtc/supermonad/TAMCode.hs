{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         TAMCode                                              *
*       Purpose:        Triangle Abstract Machine (TAM) Code                 *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2013                  *
*                                                                            *
******************************************************************************
-}

-- | Triangle Abstract Machine (TAM) Code.

module TAMCode (
    MTInt,          -- TAM integer type
    Addr(..),           -- Address
    TAMInst(..)
) where

import Control.Supermonad.Prelude

-- HMTC module imports
import Name
import Type (MTInt)


-- | TAM stack addresses
data Addr
    = SB MTInt                  -- ^ SB (Stack base) + displacement: [SB + d]
    | LB MTInt                  -- ^ LB (Local Base) + displacement: [LB + d]
    | ST MTInt                  -- ^ ST (Stack Top) + displacement:  [ST + d]
    deriving (Eq, Show)

-- | TAM instruction type.
data TAMInst
    -- Label
    = Label Name                -- ^ Symbolic location (pseudo instruction)

    -- Load and store
    | LOADL  MTInt              -- ^ Push literal integer onto stack
    | LOADCA Name               -- ^ Push code address onto stack
    | LOAD   Addr               -- ^ Push contents at addres onto stack
    | LOADA  Addr               -- ^ Push address onto stack
    | LOADI  MTInt              -- ^ Load indirectly; addr = top elem.+displ.
    | STORE  Addr               -- ^ Pop elem. from stack and store at address
    | STOREI MTInt              -- ^ Store indirectly; addr = top elem.+displ.

    -- Block operations
    | LOADLB  MTInt MTInt       -- ^ Push block of literal integer onto stack
    | LOADIB  MTInt             -- ^ Load block indirectly; addr = top elem.
    | STOREIB MTInt             -- ^ Store block indirectly; addr = top elem.
    | POP     MTInt MTInt       -- ^ POP m n: pop n elements below top m elems.
    
    -- Aritmetic operations
    | ADD                       -- ^ [b, a, ...] => [a + b, ...]
    | SUB                       -- ^ [b, a, ...] => [a - b, ...]
    | MUL                       -- ^ [b, a, ...] => [a * b, ...]
    | DIV                       -- ^ [b, a, ...] => [a / b, ...]
    | NEG                       -- ^ [a, ...]    => [-a, ...]

    -- Comparison & logical ops: false = 0, true = 1 (as arg., anything /= 0)
    | LSS                       -- ^ [b, a, ...] => [a < b, ...]
    | EQL                       -- ^ [b, a, ...] => [a == b, ...]
    | GTR                       -- ^ [b, a, ...] => [a > b, ...]
    | AND                       -- ^ [b, a, ...] => [a && b, ...]
    | OR                        -- ^ [b, a, ...] => [a || b, ...]
    | NOT                       -- ^ [a, ...]    => [!a, ...]

    -- Control transfer
    | JUMP     Name             -- ^ Jump unconditionally 
    | JUMPIFZ  Name             -- ^ Pop top value, jump if zero (false)
    | JUMPIFNZ Name             -- ^ Pop top value, jump if not zero (true)
    | CALL     Name             -- ^ Call global subroutine
    | CALLI                     -- ^ Call indirectly; addr & static lnk on stk 
    | RETURN   MTInt MTInt      -- ^ RETURN m n: result size m, args size n.

    -- I/O
    | PUTINT                    -- ^ Pop and print top element to terminal
    | PUTCHR                    -- ^ Pop and print top element interp. as char.
    | GETINT                    -- ^ Read an integer and push onto stack
    | GETCHR                    -- ^ Read a character and push onto stack

    -- TAM Control
    | HALT                      -- ^ Stop TAM
    deriving (Eq, Show)
