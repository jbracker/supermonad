{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Type                                                 *
*       Purpose:        MiniTriangle type representation                     *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- Note: The current approach to records is to ensure that the fields are
-- ordered lexicographically by field name. This means that the subexpressions
-- of a record constructor may be reordered. That in turn could affect the
-- semantics of a program if any of these subexpressions have side effects.
-- Thus, at present, the semantics is that the evaluation order among
-- field expressions is undefined (unlike e.g. the evaluation order among
-- initalization expressions in declarations, or argument evaluation).
-- Maybe revisit this design at some point. For example, if more liberal
-- operations on "sub" records are supported anyway, field manipulations
-- may be needed anyway and the extra cost of of run-time reordering might
-- be worth it to allow evaluation in the given order.
--
-- Note: The current way SomeType is handled is maybe not very systematic.
-- For some purposes (e.g. ==, <:, fldExists) SomeType is treated as if it
-- could be any type. But maybe that is ill-advised? Maybe better to have
-- special comparison operators? Or at least limit the special treatment to
-- (==) and (<:).

-- | MiniTriangle type representation and related definitions.

module Type (
    -- Representing and working with MiniTriangle types
    Type (..),          -- Type for representing MiniTriangle types.
    sortRcdFlds,        -- :: [(Name, a)] -> [(Name, a)]
    (<:),               -- :: Type -> Type -> Bool
    refType,            -- :: Type -> Bool
    rfcdType,           -- :: Type -> Type
    mapRfcdType,        -- :: (Type -> Type) -> Type -> Type
    aryType,            -- :: Type -> Bool
    eltType,            -- :: Type -> Type
    arySize,            -- :: Type -> MTInt
    rcdType,            -- :: Type -> Bool
    fldType,            -- :: Name -> Type -> Type
    fldExists,          -- :: Name -> Type -> Bool
    arrType,            -- :: Type -> Bool
    arity,              -- :: Type -> Int
    argTypes,           -- :: Type -> [Type]
    retType,            -- :: Type -> Type

    -- Representing and working with values of specific MiniTriangle types
    MTInt,              -- Haskell repr. of MiniTriangle integer values
    isMTInt,            -- :: Integer -> Bool
    MTChr,              -- Haskell repr. of MiniTriangle character values
    isMTChr             -- :: Char -> Bool
) where

import Control.Supermonad.Prelude

import Data.Int (Int32)
import Data.Char (isLatin1)
import Data.List (sortBy)

-- HMTC module imports
import Diagnostics (assert)
import Name

------------------------------------------------------------------------------
-- Representation of MiniTriangle types and related notions
------------------------------------------------------------------------------

-- | Type for representing MiniTriangle types. Type representations can
-- be compared for equality. Types are only equal if they have the same
-- representation, except that SomeType is treated as equal to any type
-- strictly as an implementation mechanism to avoid follow-on errors from
-- a detected type error.
data Type = SomeType            -- ^ Some unknown type 
          | Void                -- ^ The empty type (return type of procedures)
          | Boolean             -- ^ The Boolean type
          | Character           -- ^ The Character type
          | Integer             -- ^ The Integer type
          | Src Type            -- ^ Read-only variable reference (source)
          | Snk Type            -- ^ Write-only variable reference (sink)
          | Ref Type            -- ^ Variable reference
          | Ary Type MTInt      -- ^ Array; fields repr. element type and size
          | Rcd [(Name, Type)]  -- ^ Record type; field names and their types.
                                -- Assumed to be sorted by field names.
          | Arr [Type] Type     -- ^ Type of procedures and functions (arrow).
                                -- The fields represent the argument types and
                                -- the result type. The latter is 'Void' for
                                -- procedures.


-- | Sorts list of pairs of field names and associated data according to
-- a canonical record field order. This function is to be used for sorting
-- any (potentially) unsorted list of record fields.
sortRcdFlds :: [(Name, a)] -> [(Name, a)]
sortRcdFlds = sortBy (\na1 na2 -> compare (fst na1) (fst na2)) 


instance Eq Type where
    SomeType   == _          = True
    _          == SomeType   = True
    Void       == Void       = True
    Boolean    == Boolean    = True
    Character  == Character  = True
    Integer    == Integer    = True
    Src t1     == Src t2     = t1 == t2
    Snk t1     == Snk t2     = t1 == t2
    Ref t1     == Ref t2     = t1 == t2
    Ary t1 s1  == Ary t2 s2  = t1 == t2 && s1 == s2
    Rcd fts1   == Rcd fts2   = fts1 == fts2
    Arr ts1 t1 == Arr ts2 t2 = ts1 == ts2 && t1 == t2
    _          == _          = False 


-- | MiniTriangle Subtyping relation.
(<:) :: Type -> Type -> Bool
(Src t1)   <: (Src t2)   = t1 <: t2
(Snk t1)   <: (Snk t2)   = t2 <: t1
(Ref t1)   <: (Ref t2)   = t1 <: t2 && t2 <: t1
(Ref t1)   <: (Src t2)   = t1 <: t2
(Ref t1)   <: (Snk t2)   = t2 <: t1
Arr ts1 t1 <: Arr ts2 t2 = and [ t2 <: t1 | (t1, t2) <- zip ts1 ts2 ] 
                           && t1 <: t2
t1         <: t2         = t1 == t2


-- | Predicate that decides if the type is a reference type.
refType :: Type -> Bool
refType (Src _) = True
refType (Snk _) = True
refType (Ref _) = True
refType _       = False


-- | Referenced type: type of the reference
rfcdType :: Type -> Type
rfcdType (Src t) = t
rfcdType (Snk t) = t
rfcdType (Ref t) = t
rfcdType _       = SomeType


-- | Map referenced type (Is there a more principled approach?)
mapRfcdType :: (Type -> Type) -> Type -> Type
mapRfcdType f (Src t) = Src (f t)
mapRfcdType f (Snk t) = Snk (f t)
mapRfcdType f (Ref t) = Ref (f t)
mapRfcdType f _       = SomeType


-- | Predicate that decides if the type is an array type.
aryType :: Type -> Bool
aryType (Ary _ _) = True
aryType _         = False


-- | Type of elements of array
eltType :: Type -> Type
eltType (Ary t _) = t
eltType _           = SomeType


-- | Size of array
arySize :: Type -> MTInt
arySize (Ary _ s) = s
arySize _         = 0


-- | Predicate that decides if the type is a record type.
rcdType :: Type -> Bool
rcdType (Rcd _ ) = True
rcdType _        = False


-- Type of field of record
fldType :: Name -> Type -> Type
fldType f (Rcd fts) =
    case lookup f fts of
        Just t  -> t
        Nothing -> SomeType
fldType _ _ = SomeType


-- Is the type a record type that contain the named field?
fldExists :: Name -> Type -> Bool
fldExists _ SomeType  = True    -- As SomeType compatible with any type  
fldExists f (Rcd fts) =
    case lookup f fts of
        Just _  -> True
        Nothing -> False
fldExists _ _ = False


-- | Predicate that decides if the type is an arrow type.
arrType :: Type -> Bool
arrType (Arr _ _) = True
arrType _         = False


-- | Arity of a function type. Non-functions are seen as functions of arity 0.
arity :: Type -> Int
arity (Arr ts _) = length ts
arity t          = 0


-- | Argument types of a function type.
argTypes :: Type -> [Type]
argTypes (Arr ts _) = ts
argTypes _          = []


-- | Return type of a function type.
retType :: Type -> Type
retType (Arr _ t) = t
retType _         = SomeType


instance Show Type where
    showsPrec _ SomeType   = showString "SomeType"
    showsPrec _ Void       = showString "Void"
    showsPrec _ Boolean    = showString "Boolean"
    showsPrec _ Character  = showString "Character"
    showsPrec _ Integer    = showString "Integer"
    showsPrec d (Src t)    = showParen (d >= 9)
                                       (showString "Src " . showsPrec 9 t)
    showsPrec d (Snk t)    = showParen (d >= 9)
                                       (showString "Snk " . showsPrec 9 t)
    showsPrec d (Ref t)    = showParen (d >= 9)
                                       (showString "Ref " . showsPrec 9 t)
    showsPrec d (Ary t s)  = showParen (d > 10)
                                       (showsPrec 10 t
                                        . showChar '['
                                        . shows s
                                        . showChar ']')
    showsPrec d (Rcd fts)  = showChar '{' . showsFieldTypes fts . showChar '}'
    showsPrec d (Arr ts t) = showParen (d >= 8)
                                       (showChar '('
                                        . showsTypes ts
                                        . showString ") -> "
                                        . showsPrec 8 t)


showsTypes :: [Type] -> ShowS
showsTypes []     = id
showsTypes (t:ts) = stAuxT t ts
    where
        stAuxT t ts = shows t . stAux ts

        stAux []     = id
        stAux (t:ts) = showString ", " . stAuxT t ts


showsFieldTypes :: [(Name, Type)] -> ShowS
showsFieldTypes []         = id
showsFieldTypes (ft : fts) = sftAuxFT ft fts
    where
        sftAuxFT (f, t) fts = showString f 
                              . showString " : " 
                              . shows t
                              . sftAux fts

        sftAux []         = id
        sftAux (ft : fts) = showString ", " . sftAuxFT ft fts


------------------------------------------------------------------------------
-- Haskell types for representing data of various MiniTriangle types
------------------------------------------------------------------------------

-- | Haskell representaion of MiniTriangle integer values.
type MTInt = Int32


isMTInt :: Integer -> Bool
isMTInt n = lbMTInt <= n && n <= ubMTInt


lbMTInt = toInteger (minBound :: MTInt)
ubMTInt = toInteger (maxBound :: MTInt)


-- | Haskell representation of MiniTriangle character values
type MTChr = Char


-- The TAM at present has no specific character type, but uses MTInt to
-- represent MTChar. Thus it is assumed that an MTInt is large enough to
-- represent an MTChar. Presently: A 32-bit signed integer is large enough
-- to hold an ISO Latin 1 character (8 bits).
-- By defining isMTChar as a CAF (Constant Applicative Form) using assert,
-- this constraint will be checkd end enforced the first time isMTChar is
-- used.

isMTChr :: Char -> Bool
isMTChr = assert (0 >= lbMTInt && 255 <= ubMTInt) 
                 "Type" "isMTChar" "MTChar is too big for MTInt."
                 isLatin1
