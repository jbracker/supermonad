{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         ScopeLevel                                           *
*       Purpose:        Definition of and operation on scope level.          *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2013                         *
*                                                                            *
******************************************************************************
-}

-- | ScopeLevel: Definition of and operation on scope level

module ScopeLevel (
    ScopeLvl,           -- Scope level
    topMajScopeLvl,     -- :: Int
    topScopeLvl,        -- :: ScopeLvl
    majScopeLvl,        -- :: ScopeLvl -> Int
    minScopeLvl,        -- :: ScopeLvl -> Int
    incMajScopeLvl,     -- :: ScopeLvl -> ScopeLvl
    incMinScopeLvl      -- :: ScopeLvl -> ScopeLvl
) where


------------------------------------------------------------------------------
-- Scope level
------------------------------------------------------------------------------

-- | Scope level.

-- Pair of major (depth of procedure/function) nesting
-- and minor (depth of let-command nesting) levels.
type ScopeLvl = (Int, Int)


topMajScopeLvl :: Int
topMajScopeLvl = 0


topScopeLvl :: ScopeLvl
topScopeLvl = (topMajScopeLvl, 0)


majScopeLvl :: ScopeLvl -> Int
majScopeLvl = fst


minScopeLvl :: ScopeLvl -> Int
minScopeLvl = fst


incMajScopeLvl :: ScopeLvl -> ScopeLvl
incMajScopeLvl (majl, _) = (majl + 1, 0)


incMinScopeLvl :: ScopeLvl -> ScopeLvl
incMinScopeLvl (majl, minl) = (majl, minl + 1)
