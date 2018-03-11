{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         SrcPos                                               *
*       Purpose:        Source-code positions and related definitions        *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2012                  *
*                                                                            *
******************************************************************************
-}

-- |Source-code positions and related definitions

module SrcPos (
    SrcPos (..),        -- Not abstract. Instances: Eq, Ord, Show.
    HasSrcPos (..)
) where

-- | Representation of source-code positions
data SrcPos
    = NoSrcPos                  -- ^ Unknown source-code position
    | SrcPos {
          spLine :: Int,        -- ^ Line number
          spCol  :: Int         -- ^ Character column number
      }
    deriving (Eq, Ord)


instance Show SrcPos where
    showsPrec _ NoSrcPos = showString "unknown position"
    showsPrec _ (SrcPos {spLine = l, spCol = c }) =
        showString "line "
        . shows l
        . showString ", column "
        . shows c

-- | Class of types that have a source-code position as a stored or computed
-- attribute.
class HasSrcPos a where
    srcPos :: a -> SrcPos


-- A list of entities that have source positions also has a source position. 
instance HasSrcPos a => HasSrcPos [a] where
    srcPos []    = NoSrcPos
    srcPos (x:_) = srcPos x


