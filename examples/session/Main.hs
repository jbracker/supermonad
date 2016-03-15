
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import Prelude
  ( String, IO, Int
  , ($), (==), (-)
  , fromInteger
  , putStrLn )

import Control.Monad
  ( Monad (..) )

import Control.Monad.Indexed
  ( IxMonad(..), (>>>=) )

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

type Ping = Eps :+: (String :!: String :?: Var Z)
type Pong = Eps :&: (String :?: String :!: Var Z)

ping :: Int -> Session (Cap (Ping, ()) Ping) () ()
pong :: Session (Cap (Pong, ()) Pong) () ()
main :: IO ()

main = do
  rv <- newRendezvous
  _ <- forkIO $ accept rv
              $ enter >>>= \_ -> ping 3
  request rv $ enter >>>= \_ -> pong

ping 0 = do
    sel1; close
  where ma >> mb = ma >>>= \_ -> mb
ping n = do
    sel2; send "Ping"
    rsp <- recv
    io $ putStrLn rsp
    zero; ping (n - 1)
  where (>>=) = (>>>=)
        ma >> mb = ma >>>= \_ -> mb

pong = offer close $ do
    rsp <- recv
    io $ putStrLn rsp
    send "Pong"
    zero; pong
  where (>>=) = (>>>=)
        ma >> mb = ma >>>= \_ -> mb


{-
main = do
  rv <- newRendezvous
  _ <- forkIO $ accept rv
              $ enter >> ping 3
  request rv $ enter >> pong

ping 0 = do
    sel1; close
ping n = do
    sel2; send "Ping"
    rsp <- recv
    io $ putStrLn rsp
    zero; ping (n - 1)

pong = offer close $ do
    rsp <- recv
    io $ putStrLn rsp
    send "Pong"
    zero; pong

-}
