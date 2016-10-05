
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

-- Requires for instances
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Use the polymonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Remove this so compilation creates a proper executable.
--module MainPolymonad ( main ) where

import Control.Supermonad.Prelude


import Control.Monad.Indexed
  ( IxPointed(..), (>>>=) )
import Data.Functor.Indexed
  ( IxApplicative(..) )
import Control.Monad.Trans.State ( StateT(..) ) 

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

liftS :: ( BindCts (Session i j) (Session j j) (Session i j)
         --, ReturnCts (Session j j)
         ) => Session i j a -> StateT s (Session i j) a
liftS sess = StateT $ \s -> sess >>= \a -> return (a, s)

liftOffer :: StateT st (Session (Cap e r) u) a 
          -> StateT st (Session (Cap e s) u) a 
          -> StateT st (Session (Cap e (r :&: s)) u) a
liftOffer ma mb = StateT $ \s -> offer (runStateT ma s) (runStateT mb s)

modify :: ( Return m, ReturnCts m
          ) => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return ((), f s)
  

main :: IO ()
main = do
  rv <- newRendezvous
  _ <- forkIO $ do
    res <- accept rv $ runStateT (liftS enter >> ping 3) 0
    putStrLn $ "Server: " ++ show res
  res <- request rv $ runStateT (liftS enter >> pong) 0
  putStrLn $ "Client: " ++ show res

-- Use an Int state to count how often the send command is called.

ping :: Int -> StateT Int (Session (Cap (Ping, ()) Ping) ()) ()
ping 0 = do
  liftS $ sel1
  liftS $ close
ping n = do
  liftS $ sel2
  liftS $ send "Ping"
  modify (+1)
  rsp <- liftS $ recv
  liftS $ io $ putStrLn rsp
  liftS $ zero
  ping (n - 1)

pong :: StateT Int (Session (Cap (Pong, ()) Pong) ()) ()
pong = liftOffer (liftS close) $ do
    rsp <- liftS $ recv
    liftS $ io $ putStrLn rsp
    liftS $ send "Pong"
    modify (+1)
    liftS $ zero
    pong

