 
module Main ( main ) where

import System.IO 
  ( hSetBuffering, stdout
  , BufferMode(NoBuffering) )

import Control.Monad ( when )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TVar 
  ( newTVarIO, readTVar, modifyTVar )

import Types ( User, Connection, mkConnection )
import Server ( server )
import Client 
  ( clientShell
  , BotClient, mkBotClient
  , sendMessageBot, terminateBot )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  
  bots <- sequence 
    $ fmap (\bot -> mkConnection >>= \conn -> bot conn >>= \b -> return (conn, b) ) 
    $ [ responseBot "RB1"
      , rageBot "Rage" ]
  
  clientConn <- mkConnection
  
  _serverThread <- forkIO $ do
    _serverEnv <- server $ clientConn : fmap fst bots
    return ()
  
  clientShell clientConn

-- | Never use more then one!
--   Weirdly, he does not respond to users that begin with the letter R.
responseBot :: User -> Connection -> IO BotClient
responseBot name conn = mkBotClient conn name
  (const $ return ())
  (\bot u -> sendMessageBot bot $ "Goodbye, " ++ u ++ "!")
  (\bot u msg -> when (head u /= 'R') $ do
    if msg == ("die " ++ name) then do
      sendMessageBot bot $ "I obey and die now..."
      terminateBot bot
    else do
      sendMessageBot bot $ "I don't understand but I still respond, " ++ u ++ "!"
  )
  (const $ return ())

rageBot :: User -> Connection -> IO BotClient
rageBot name conn = do
  counterVar <- newTVarIO (0 :: Int)
  mkBotClient conn name
    (const $ return ())
    (\bot u -> sendMessageBot bot $ "Finally you are gone, " ++ u ++ "!")
    (\bot u _msg -> do
      counter <- atomically $ readTVar counterVar
      if counter >= 5 then do
        sendMessageBot bot $ "I have had enough of this, " ++ u ++ "!"
        terminateBot bot
      else do
        sendMessageBot bot $ "Be quiet, " ++ u ++ "!"
        atomically $ modifyTVar counterVar (+1)
    )
    (const $ return ())





