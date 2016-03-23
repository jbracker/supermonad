{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         Token                                                *
*       Purpose:        Representation of tokens (lexical symbols)           *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2015                  *
*                                                                            *
******************************************************************************
-}

-- | Representation of tokens (lexical symbols).

module Token where

-- HMTC module imports
import Name


-- | Token type.

data Token
    -- Graphical tokens
    = LPar      -- ^ \"(\"
    | RPar      -- ^ \")\"
    | LBrk      -- ^ \"[\"
    | RBrk      -- ^ \"]\"
    | LBrc      -- ^ \"{\"
    | RBrc      -- ^ \"}\"
    | Comma     -- ^ \",\"
    | Period    -- ^ \".\"
    | Semicol   -- ^ \";\"
    | Colon     -- ^ \":\"
    | ColEq     -- ^ \":=\"
    | Equals    -- ^ \"=\"
    | Cond      -- ^ \"?\"

    -- Keywords
    | Begin             -- ^ \"begin\"
    | Const             -- ^ \"const\"
    | Do                -- ^ \"do\"
    | Else              -- ^ \"else\"
    | Elsif             -- ^ \"elsif\"
    | End               -- ^ \"end\"
    | Fun               -- ^ \"fun\"
    | If                -- ^ \"if\"
    | In                -- ^ \"in\"
    | Let               -- ^ \"let\"
    | Out               -- ^ \"out\"
    | Overloaded        -- ^ \"overloaded\"
    | Proc              -- ^ \"proc\"
    | Repeat            -- ^ \"repeat\"
    | Then              -- ^ \"then\"
    | Until             -- ^ \"until\"
    | Var               -- ^ \"var\"
    | While             -- ^ \"while\"

    -- Tokens with variable spellings
    | LitInt {liVal :: Integer}         -- ^ Integer literals
    | LitChr {lcVal :: Char}            -- ^ Character literals
    | Id     {idName :: Name}           -- ^ Identifiers
    | Op     {opName :: Name}           -- ^ Operators

    -- End Of File marker
    | EOF                               -- ^ End of file (input) marker.
    deriving (Eq, Show)
