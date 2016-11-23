
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

-- Requires for instances
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Use the supermonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Remove this so compilation creates a proper executable.
--module MainPolymonad ( main ) where

import Control.Supermonad.Prelude


import Control.Monad.Indexed
  ( IxPointed(..), (>>>=) )
import Data.Functor.Indexed
  ( IxApplicative(..) )

import Control.Concurrent
  ( forkIO )

import Control.Concurrent.SimpleSession.Implicit
  ( Session, Cap
  , io, send, recv, close, sel1, sel2, zero, offer, enter
  , newRendezvous, accept, request )
import Control.Concurrent.SimpleSession.SessionTypes
  ( Var, Eps
  , (:&:), (:+:), (:!:), (:?:)
  , Z )

instance Functor (Session i j) where
  fmap = fmap

instance Applicative (Session i j) (Session j k) (Session i k) where
  (<*>) = iap
  
instance Bind (Session i j) (Session j k) (Session i k) where
  (>>=) = (>>>=)

instance Return (Session i i) where
  return = ireturn

instance Fail (Session i j) where
  fail = error

type Ping = Eps :+: (String :!: String :?: Var Z)
type Pong = Eps :&: (String :?: String :!: Var Z)

main :: IO ()
main = do
  rv <- newRendezvous
  _ <- forkIO $ accept rv
              $ enter >> ping 3
  request rv $ enter >> pong

ping :: Int -> Session (Cap (Ping, ()) Ping) () ()
ping 0 = do
    sel1; close
ping n = do
    sel2; send "Ping"
    rsp <- recv
    io $ putStrLn rsp
    zero; ping (n - 1)

pong :: Session (Cap (Pong, ()) Pong) () ()
pong = offer close $ do
    rsp <- recv
    io $ putStrLn rsp
    send "Pong"
    zero; pong

