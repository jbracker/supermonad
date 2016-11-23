
-- Use the supermonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Monad.Plugin #-}

import Control.Supermonad.Prelude

main :: IO ()
main = return ()
