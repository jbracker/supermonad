 
module Main ( main ) where

import System.IO 
  ( hSetBuffering, stdout
  , BufferMode(NoBuffering) )

import Control.Concurrent ( forkIO )

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
    $ [ responseBot "RB1" ]
  
  clientConn <- mkConnection
  
  _serverThread <- forkIO $ do
    _serverEnv <- server $ clientConn : fmap fst bots
    return ()
  
  clientShell clientConn

-- | Never use more then one!
responseBot :: User -> Connection -> IO BotClient
responseBot name conn = mkBotClient conn name
  (const $ return ())
  (\bot u -> sendMessageBot bot $ "Goodbye, " ++ u ++ "!")
  (\bot u msg -> do
    if msg == ("die " ++ name) then do
      sendMessageBot bot $ "I obey and die now..."
      terminateBot bot
    else do
      sendMessageBot bot $ "I don't understand but I still respond, " ++ u ++ "!")
  (const $ return ())
  
  {-
  
  mkBotClient :: User -- ^ The bots username.
            -> (BotClient -> IO ()) -- ^ Executed when there are no updates.
            -> (BotClient -> User -> IO ()) -- ^ Executed when a user leaves.
            -> (BotClient -> User -> Message -> IO ()) -- ^ Excuted when a new message arives.
            -> (BotClient -> IO ()) -- ^ Executed when the bot terminates. This may happen through the server or the bot itself.
            -> IO BotClient
            -}