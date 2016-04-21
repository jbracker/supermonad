
-- Requires for instances
{-# LANGUAGE MultiParamTypeClasses #-}

-- Use the polymonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

-- Remove this so compilation creates a proper executable.
--module MainSet ( main ) where

import Control.Supermonad.Prelude

import Data.Set ( Set )
import qualified Data.Set as S

bindSet :: Ord b => Set a -> (a -> Set b) -> Set b
bindSet s f = S.foldr S.union S.empty $ S.map f s

returnSet :: a -> Set a
returnSet = S.singleton

instance Functor Set where
  fmap = undefined -- TODO

instance Bind Set Set Set where
  (>>=) = undefined -- TODO

instance Return Set where
  return = returnSet

instance Fail Set where
  fail = error


main :: IO ()
main = do
  putStrLn $ show $ do
    a <- S.fromList ['a', 'b', 'c']
    b <- S.fromList ['a', 'b', 'c']
    c <- S.fromList ['a', 'b', 'c']
    return [a, b, c]
