
-- Requires for instances
{-# LANGUAGE MultiParamTypeClasses #-}

-- Use the polymonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

-- Remove this so compilation creates a proper executable.
--module MainSet ( main ) where

import Control.Supermonad.Constrained.Prelude

import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn $ show $ do
    let s = S.fromList ['a', 'b', 'c']
    a <- s
    b <- s
    c <- fmap (const 'd') s
    return [a, b, c]
