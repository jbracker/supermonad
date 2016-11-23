{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         LibMT                                                *
*       Purpose:        TAM code for LibMT: the MiniTriangle std library.    *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | LibMT: MiniTriangle Standard Library

module LibMT (
    libMT       -- :: [TAMInst]
) where

import Control.Supermonad.Prelude

import Data.Char (ord)

-- HMTC module imports
import Name
import TAMCode


-- | The MiniTriangle standard library: code for functions and procedures
-- declared in the standard environment as well as for internal use by the
-- TAM \"runtime\" system. Note that the code generator makes assumptions
-- about the meaning of many of these routines (\"add\" etc.) based on their
-- names for optimization purposes. 

libMT :: [TAMInst]
libMT = [
-- preinc
    Label "preinc",
    LOAD (LB (-1)),
    LOADI 0,
    LOADL 1,
    ADD,
    LOAD (ST (-1)),
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 1 1,

-- predec
    Label "predec",
    LOAD (LB (-1)),
    LOADI 0,
    LOADL 1,
    SUB,
    LOAD (ST (-1)),
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 1 1,

-- postinc
    Label "postinc",
    LOAD (LB (-1)),
    LOADI 0,
    LOAD (ST (-1)),
    LOADL 1,
    ADD,
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 1 1,

-- postdec
    Label "postdec",
    LOAD (LB (-1)),
    LOADI 0,
    LOAD (ST (-1)),
    LOADL 1,
    SUB,
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 1 1,

-- add
    Label "add",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    ADD,
    RETURN 1 2,

-- sub
    Label "sub",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    SUB,
    RETURN 1 2,

-- mul
    Label "mul",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    MUL,
    RETURN 1 2,

-- div
    Label "div",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    DIV,
    RETURN 1 2,

-- pow
    Label "pow",
    LOADL 1,
    Label "pow_loop",
    LOAD (LB (-1)),
    LOADL 0,
    GTR,
    JUMPIFZ "pow_out",
    LOAD (LB (-1)),
    LOADL 1,
    SUB,
    STORE (LB (-1)),
    LOAD (LB (-2)),
    MUL,
    JUMP "pow_loop",
    Label "pow_out",
    RETURN 1 2,

-- neg
    Label "neg",
    LOAD (LB (-1)),
    NEG,
    RETURN 1 1,

-- lt
    Label "lt",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    LSS,
    RETURN 1 2,

-- le
    Label "le",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    GTR,
    NOT,
    RETURN 1 2,

-- eq
    Label "eq",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    EQL,
    RETURN 1 2,

-- ne
    Label "ne",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    EQL,
    NOT,
    RETURN 1 2,

-- ge
    Label "ge",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    LSS,
    NOT,
    RETURN 1 2,

-- gt
    Label "gt",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    GTR,
    RETURN 1 2,

-- and
    Label "and",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    AND,
    RETURN 1 2,

-- or
    Label "or",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    OR,
    RETURN 1 2,

-- not
    Label "not",
    LOAD (LB (-1)),
    NOT,
    RETURN 1 1,

-- getchr
    Label "getchr",
    GETCHR,
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 0 1,

-- putchr
    Label "putchr",
    LOAD (LB (-1)),
    PUTCHR,
    RETURN 0 1,

-- getint
    Label "getint",
    GETINT,
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 0 1,

-- putint
    Label "putint",
    LOAD (LB (-1)),
    PUTINT,
    RETURN 0 1,

-- skip
    Label "skip",
    RETURN 0 0,

-- ixerror
    Label "ixerror",
    LOADL (chr 'I'),
    PUTCHR,
    LOADL (chr 'x'),
    PUTCHR,
    LOADL (chr ':'),
    PUTCHR,
    LOADL (chr ' '),
    PUTCHR,
    LOAD (LB (-1)),
    PUTINT,
    HALT
    ]


chr :: Char -> MTInt
chr c = fromIntegral (ord c)
