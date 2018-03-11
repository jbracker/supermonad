
{-# LANGUAGE GADTs #-}

module Test.Utils 
  ( Test(..)
  , runTest, runTests 
  ) where

import System.Exit ( exitFailure )

import Test.QuickCheck

data Test where
  Test :: (Testable p) => String -> p -> Test

runTests :: [Test] -> IO ()
runTests = sequence_ . fmap runTest

runTest :: Test -> IO ()
runTest (Test name p) = do
  putStrLn $ "+++ " ++ name
  res <- quickCheckResult p
  case res of
    Success {} -> return ()
    GaveUp {}  -> return ()
    _ -> exitFailure
