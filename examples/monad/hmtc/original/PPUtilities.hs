{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPUtilities                                          *
*       Purpose:        Pretty-printing utilities                            *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2012                  *
*                                                                            *
******************************************************************************
-}

-- | Pretty-printing utilities.

module PPUtilities (
    ppName,     -- :: Name -> ShowS
    ppSrcPos,   -- :: SrcPos -> ShowS
    ppOpt,      -- :: Int -> (Int -> a -> ShowS) -> Maybe a -> ShowS
    ppSeq,      -- :: Int -> (Int -> a -> ShowS) -> [a] -> ShowS
    indent,     -- :: Int -> ShowS
    nl,         -- :: ShowS
    spc,        -- :: ShowS
    spcs,       -- :: Int -> ShowS
    leftJust,   -- :: Int -> String -> ShowS
    rightJust   -- :: Int -> String -> ShowS
) where

-- HMTC module imports
import Name (Name)
import SrcPos (SrcPos)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Pretty-prints a name.
ppName :: Name -> ShowS
ppName n = showChar '\"' . showString n . showChar '\"'


-- | Pretty-prints a source code position.
ppSrcPos :: SrcPos -> ShowS
ppSrcPos sp = showChar '<' . showString (show sp) . showChar '>'


-- | Pretty-prints an optional item. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pretty-printing function for the item.
--
-- (3) The optional item to print.
ppOpt :: Int -> (Int -> a -> ShowS) -> Maybe a -> ShowS
ppOpt _ _  Nothing  = id
ppOpt n pp (Just x) = pp n x


-- | Pretty-prints a sequence of items. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pretty-printing function for each item.
--
-- (3) Sequence of items to print.
ppSeq :: Int -> (Int -> a -> ShowS) -> [a] -> ShowS
ppSeq _ _  []     = id
ppSeq n pp (x:xs) = pp n x . ppSeq n pp xs


-- | Indent to specified level by printing spaces.
indent :: Int -> ShowS
indent n = spcs (2 * n)


-- | Start a new line.
nl :: ShowS
nl  = showChar '\n'


-- | Print a space.
spc :: ShowS
spc = showChar ' '


-- | Print n spaces.
spcs :: Int -> ShowS
spcs n = showString (take n (repeat ' '))


-- | Left justify in field of width n
leftJust :: Int -> String -> ShowS
leftJust n s = showString s . spcs (max 0 (n - length s))
             

-- | Right justify in field of width n
rightJust :: Int -> String -> ShowS
rightJust n s = spcs (max 0 (n - length s)) . showString s
