{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Name                                                 *
*       Purpose:        Representation of names                              *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2012                  *
*                                                                            *
******************************************************************************
-}

-- | Representation of names. Types, variables, procedures, operators ...

module Name where

import Control.Supermonad.Prelude

type Name = String
