
-- | Utility functions useful for debugging the plugin. Some of these 
--   may be unsafe to use in general.
module Control.Supermonad.Plugin.Debug
  ( containsAnyOf
  , containsAllOf
  , containsNoneOf
  , pprToStr, sDocToStr
  ) where

import Data.List ( isInfixOf )

import Outputable
  ( Outputable(..), SDoc
  , showSDocUnsafe )

-- | Convert some generic outputable to a string (potentially unsafe).
pprToStr :: Outputable o => o -> String
pprToStr = sDocToStr . ppr

-- | Convert an 'SDoc' to a string (potentially unsafe).
sDocToStr :: SDoc -> String
sDocToStr = showSDocUnsafe

-- | Check if the string reprentation of the given 'Outputable'
--   contains __any__ of the given strings.
containsAnyOf :: (Outputable o) => o -> [String] -> Bool
containsAnyOf obj = any (`isInfixOf` pprToStr obj)

-- | Check if the string reprentation of the given 'Outputable'
--   contains __all__ of the given strings.
containsAllOf :: (Outputable o) => o -> [String] -> Bool
containsAllOf obj = all (`isInfixOf` pprToStr obj)

-- | Check if the string reprentation of the given 'Outputable'
--   contains __none__ of the given strings.
containsNoneOf :: (Outputable o) => o -> [String] -> Bool
containsNoneOf obj = not . (obj `containsAnyOf`)
