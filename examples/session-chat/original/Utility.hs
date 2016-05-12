 
module Utility 
  ( ifThenElse
  , stm
  ) where

import Control.Concurrent.STM 
  ( STM, atomically )

import Control.Concurrent.SimpleSession.Implicit 
  ( Session, io )

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

stm :: STM a -> Session s s a
stm = io . atomically