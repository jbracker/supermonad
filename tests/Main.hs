
module Main ( main ) where
{-
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Super.Plugin.Collection.Map as UM
import qualified Control.Super.Plugin.Collection.Set as US
-}
import Test.Utils
import qualified Test.Control.Super.Plugin.Collection.Map as Map
import qualified Test.Control.Super.Plugin.Collection.Set as Set

main :: IO ()
main = do
  putStrLn "=== Set Tests ==="
  runTests (Set.tests)
  putStrLn "=== Map Tests ==="
  runTests (Map.tests)
  





















