{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         MTStdEnv                                             *
*       Purpose:        MiniTriangle Initial Environment                     *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | MiniTriangle initial environment

module MTStdEnv (
    Env,        -- Re-exported
    mtStdEnv    -- :: Env
) where

import Control.Supermonad.Prelude


-- HMTC module imports
import Name
import TAMCode (MTInt)
import Type
import Symbol (ExtSymVal (..))
import Env


-- | The MiniTriangle initial environment.
--
-- [Types:] Boolean, Character, Integer
--
-- [Constants:]
--
--   * false, true : Boolean
--
--   * minint, maxint : Integer
--
-- [Functions (binary (infix) and unary (prefix/postfix) operators):]
--
--   * (++_), (--_), (_++), (_--) : (Ref Integer) -> Integer
--
--   * (_+_), (_-_), (_*_), (_\/_), (_\^_) : (Integer, Integer) -> Integer
--
--   * (-_) : Integer -> Integer
--
--   * (_\<_), (_\<=_), (_==_), (_!=_), (_>=_), (_>_) : 
--       (Integer, Integer) -> Boolean,
--       (Boolan, Boolean) -> Boolean
--
--   * (_&&_), (_||_) : (Boolean, Boolean) -> Boolean
--
--   * (!_) : Boolean -> Boolean
--
-- [Procedures:]
--
--   * getchr : (Snk Character) -> Void
--
--   * putchr : Character -> Void
--
--   * getint : (Snk Integer) -> Void
--
--   * putint : Integer -> Void
--
--   * skip : () -> Void
--
-- Note the naming convention for infix/prefix/postfix operators with
-- underscore indicating the argument position(s). Parser assumes this.
-- Note that labels have to agree with the code in "LibMT".

mtStdEnv :: Env
mtStdEnv =
    mkTopLvlEnv
        [("Boolean", Boolean),
         ("Character", Character),
         ("Integer", Integer)]
        [("false",   Boolean, ESVBool False),
         ("true",    Boolean, ESVBool True),
         ("minint",  Integer, ESVInt (minBound :: MTInt)),
         ("maxint",  Integer, ESVInt (maxBound :: MTInt)),
         ("++_",     Arr [Ref Integer] Integer,      ESVLbl "preinc"),
         ("--_",     Arr [Ref Integer] Integer,      ESVLbl "predec"),
         ("_++",     Arr [Ref Integer] Integer,      ESVLbl "postinc"),
         ("_--",     Arr [Ref Integer] Integer,      ESVLbl "postdec"),
         ("_+_",     Arr [Integer, Integer] Integer, ESVLbl "add"),
         ("_-_",     Arr [Integer, Integer] Integer, ESVLbl "sub"),
         ("_*_",     Arr [Integer, Integer] Integer, ESVLbl "mul"),
         ("_/_",     Arr [Integer, Integer] Integer, ESVLbl "div"),
         ("_^_",     Arr [Integer, Integer] Integer, ESVLbl "pow"),
         ("-_",      Arr [Integer] Integer,          ESVLbl "neg"),
         -- The comparison operators are overloaded, but their impl. shared.
         ("_<_",     Arr [Integer, Integer] Boolean, ESVLbl "lt"),
         ("_<=_",    Arr [Integer, Integer] Boolean, ESVLbl "le"),
         ("_==_",    Arr [Integer, Integer] Boolean, ESVLbl "eq"),
         ("_!=_",    Arr [Integer, Integer] Boolean, ESVLbl "ne"),
         ("_>=_",    Arr [Integer, Integer] Boolean, ESVLbl "ge"),
         ("_>_",     Arr [Integer, Integer] Boolean, ESVLbl "gt"),
         ("_<_",     Arr [Boolean, Boolean] Boolean, ESVLbl "lt"),
         ("_<=_",    Arr [Boolean, Boolean] Boolean, ESVLbl "le"),
         ("_==_",    Arr [Boolean, Boolean] Boolean, ESVLbl "eq"),
         ("_!=_",    Arr [Boolean, Boolean] Boolean, ESVLbl "ne"),
         ("_>=_",    Arr [Boolean, Boolean] Boolean, ESVLbl "ge"),
         ("_>_",     Arr [Boolean, Boolean] Boolean, ESVLbl "gt"),
         ("_&&_",    Arr [Boolean, Boolean] Boolean, ESVLbl "and"),
         ("_||_",    Arr [Boolean, Boolean] Boolean, ESVLbl "or"),
         ("!_",      Arr [Boolean] Boolean,          ESVLbl "not"),
         ("getchr",  Arr [Snk Character] Void,       ESVLbl "getchr"), 
         ("putchr",  Arr [Character] Void,           ESVLbl "putchr"),
         ("getint",  Arr [Snk Integer] Void,         ESVLbl "getint"), 
         ("putint",  Arr [Integer] Void,             ESVLbl "putint"),
         ("skip",    Arr [] Void,                    ESVLbl "skip")]
