 
module Utility 
  ( ifThenElse
  , stm
  , trim
  , isValidUserName
  ) where

import Data.Char ( isSpace, isAscii, isAlpha, isNumber )
import Data.List ( dropWhileEnd )

import Control.Concurrent.STM 
  ( STM, atomically )

import Control.Concurrent.SimpleSession.Implicit 
  ( Session, io )

-- | Standard semantics for if-then-else to be used with rebindable syntax.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

-- | Execute an atomic portion of 'STM' within the 'Session' monad.
stm :: STM a -> Session s s a
stm = io . atomically

-- | Remove whitespaces from beginning and end of string.
trim :: String -> String
trim str = dropWhile isSpace $ dropWhileEnd isSpace str

-- | Check if the given username is valid.
isValidUserName :: String -> Bool
isValidUserName = all (\c -> isAscii c && (isAlpha c || isNumber c))