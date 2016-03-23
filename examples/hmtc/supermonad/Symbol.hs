{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Symbol                                               *
*       Purpose:        Symbols: information about named entities.           *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Symbols: information about named entities. Symbols are used to keep
-- track of information about defined and declared named entities, like
-- variables, procedures, functions, and types. They are used in the symbol
-- table and MTIR.
--
-- There are two kinds of symbols: term-level symbols, denoting entities like
-- variables and procedures, and type-level symbols, denoting types.

module Symbol (
    -- Auxiliary types
    ExtSymVal (..),     -- External symbol value

    -- Symbol types
    TermSym,            -- Union of ExtTermSym and IntTermSym
    ExtTermSym (..),    -- Not abstract. Instances: Eq, Show, HasSrcPos.
    IntTermSym (..),    -- Not abstract. Instances: Eq, Show, HasSrcPos.
    TypeSym (..),       -- Not abstract. Instances: Eq, Show, HasSrcPos.

    -- Access of fields common to all term symbols
    tmsName,            -- TermSym -> Name
    tmsType             -- TermSym -> Type
) where

import Control.Supermonad.Prelude


-- HMTC module imports
import Name
import SrcPos
import ScopeLevel
import Type
import TAMCode (MTInt)


------------------------------------------------------------------------------
-- External symbol value
------------------------------------------------------------------------------

-- External symbol value
data ExtSymVal
   = ESVBool Bool
   | ESVInt MTInt
   | ESVChar Char
   | ESVLbl Name
   deriving Show


------------------------------------------------------------------------------
-- Term-level symbol
------------------------------------------------------------------------------

-- [For future reference:
--
-- Term- and type-level symbols are defined in the same module since they
-- probably would end up being mutually recursive if the compiler is extended
-- to support user-defined types. For example, an enumeration type symbol
-- might refer to its elements, which would be terms, and vice versa.]

-- ExtTermSym and IntTermSym used to be two constructors for the same type.
-- But making them distinct allows for more precise types. However, does
-- complicate MTIR a bit (the constructor ExpExtRef, which can be seen as a
-- variant of ExpVar), and maybe internal symbols should carry optional values
-- at some point, making them even more similar to external symbols.]

-- | Term-level symbol. Symbol denoting a term-level named entity, like
-- a procedures, variable, or operator.

-- There are two kinds of term-level symbols: external and internal.
-- Both have name, possibly an instance number, and type.
-- 
-- The instance number is used to distinguish between overloaded symbols;
-- i.e., symbols with the same name. Each instance number must be distinct
-- among the instance numbers for all symbols sharing a name. A symbol without
-- an instance number is not overloaded.
--
-- External symbols are defined outside the current compilation unit (e.g.
-- references to a library). An external symbols has a value, but no source
-- code position (as source code position here only refers to the current
-- compilation unit).
--
-- Internal symbols are defined in the current compilation unit (e.g.
-- variables defined in a let-command, local procedures/functions when
-- supported). An internal symbol has a scope level and an associated source
-- code position referring to where the entity was defined. However, an
-- internal symbol has no value as the value will only become known during
-- code generation in general. (Of course, some values might be known at
-- compile time; e.g., as the result of constant propagation. Extending
-- internal symbols with an optional value field could be one way of keeping
-- track of such information. But this could also be done through a separate
-- environment mapping "known" internal symbols to their values.)

type TermSym = Either ExtTermSym IntTermSym


tmsName :: TermSym -> Name
tmsName tms = either etmsName itmsName tms


tmsType :: TermSym -> Type
tmsType tms = either etmsType itmsType tms


data ExtTermSym
    -- | External symbol: defined outside current compilation unit
    = ExtTermSym {
          etmsName :: Name,             -- ^ Name
          etmsInst :: Maybe Int,        -- ^ Optional instance number
          etmsType :: Type,             -- ^ Type of the symbol
          etmsVal  :: ExtSymVal         -- ^ Value of the symbol
      }


-- As external term-level symbols only are defined at the top level, they are
-- uniquely identified by their name and instance number (if any), and can
-- thus be compared for equality by just comparing names and any instance
-- numbers..

instance Eq ExtTermSym where
    (==) (ExtTermSym {etmsName = n1, etmsInst = mi1})
         (ExtTermSym {etmsName = n2, etmsInst = mi2}) =
         n1 == n2 && mi1 == mi2


instance Show ExtTermSym where
    showsPrec d (ExtTermSym {etmsName = n, etmsInst = mi,
                             etmsType = t, etmsVal = v}) =
        showParen (d >= 1)
                  (showChar '\"' . showString n
                   . maybe id (\i -> showChar '#' . shows i) mi 
                   . showChar '@' . showString "(external)" . showChar '\"'
                   . showString " : " . shows t
                   . showString " = " . shows v)


instance HasSrcPos ExtTermSym where
    srcPos (ExtTermSym {}) = NoSrcPos


data IntTermSym
    -- | Internal symbol: defined in current compilation unit
    = IntTermSym { 
          itmsLvl    :: ScopeLvl,       -- ^ Scope level
          itmsName   :: Name,           -- ^ Name
          itmsInst   :: Maybe Int,      -- ^ Optional instance number
          itmsType   :: Type,           -- ^ Type of the symbol
          itmsSrcPos :: SrcPos          -- ^ Source code position of the
                                        --   declaration or definition.
      }


-- Within a scope, term-level symbols are uniquely identified by their scope
-- level and name. Term-level symbols can thus be compared for equality
-- in that way.

instance Eq IntTermSym where
    (==) (IntTermSym {itmsLvl = l1, itmsInst = mi1, itmsName = n1})
         (IntTermSym {itmsLvl = l2, itmsInst = mi2, itmsName = n2}) =
         l1 == l2 && mi1 == mi2 && n1 == n2


instance Show IntTermSym where
    showsPrec d (IntTermSym {itmsLvl = l, itmsName = n, itmsInst = mi,
                             itmsType = t}) =
        showParen (d >= 1)
                  (showChar '\"' . showString n 
                   . maybe id (\i -> showChar '#' . shows i) mi 
                   . showChar '@' . shows l . showChar '\"'
                   . showString " : " . shows t)


instance HasSrcPos IntTermSym where
    srcPos (IntTermSym {itmsSrcPos = sp}) = sp


------------------------------------------------------------------------------
-- Type-level symbol
------------------------------------------------------------------------------

-- | Type-level symbol. Symbols denoting a type. Can only be defined at the
-- top level.

-- The present representation only allows user-defined types in the form
-- of simple type synonyms. Extensions would be needed beyond this
-- to handle enumeration types, records, algebraic types, parameterized
-- types, etc..

-- ToDo: Distinguish between internal and external type-level symbols?
-- The latter would not have any source code position.

data TypeSym = TypeSym {
                   tpsName   :: Name,    -- ^ Name
                   tpsType   :: Type,    -- ^ Representation of the type
                   tpsSrcPos :: SrcPos   -- ^ Source code position of the
                                         -- type definition
               }


-- As type-level symbols only are defined at the top level, they are
-- uniquely identified by their name and can thus be compared for equality
-- by just comparing the names..

instance Eq TypeSym where
    TypeSym {tpsName = n1} == TypeSym {tpsName = n2}
        | n1 == n2 = True
    _ == _ = False


instance Show TypeSym where
    showsPrec d (TypeSym {tpsName = n, tpsType = t}) =
        showParen (d >= 1)
                  (showString n . showString " = " . shows t)


instance HasSrcPos TypeSym where
    srcPos = tpsSrcPos
