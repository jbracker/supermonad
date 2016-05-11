
{-# LANGUAGE RebindableSyntax #-}

module Server ( server ) where

import Prelude

import Control.Monad ( void, when, forM )

import Control.Monad.Indexed ( (>>>=), ireturn )

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, STM )
import Control.Concurrent.STM.TMVar
  ( TMVar
  , newEmptyTMVarIO
  , takeTMVar, readTMVar, putTMVar
  , tryTakeTMVar, tryReadTMVar, tryPutTMVar
  )
import Control.Concurrent.STM.TVar 
  ( TVar
  , newTVarIO
  , readTVar, writeTVar, modifyTVar
  )

import Control.Concurrent.SimpleSession.Implicit 
  ( Session, Cap
  , Rendezvous
  , Rec, Var
  , Z, S
  , accept
  , io
  , recv, send
  , sel1, sel2
  , close
  , enter, suc, zero
  , Eps, Rec, (:+:), (:&:), (:!:), (:?:) 
  )

import Types 
  ( User, Message
  , ServerCommand(..), ClientCommand(..)
  , ServerInput, ServerInputLoop
  , ServerOutput, ServerOutputLoop
  )

data ServerEnvironment = ServerEnvironment
  { serverCommNodes :: TVar [ServerCommNode]
  , serverShutdown :: TVar Bool
  }

data Connection = Connection 
  { serverInputConn  :: Rendezvous ServerInput
  , serverOutputConn :: Rendezvous ServerOutput
  }

data ServerCommNode = ServerCommNode 
  { commNodeSendCommand :: TMVar ServerCommand
  , commNodeRecvCommand :: TMVar ClientCommand
  , commNodeTerminated :: TVar Bool
  , commNodeUserName :: User
  }

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

server :: [Connection] -> IO ServerEnvironment
server conns = do
  shutdownVar <- newTVarIO False
  commNodesVar <- newTVarIO []
  
  forkIO $ do
    forM conns $ \conn -> do
      commNode <- spawnServerCommNode (lockUserName commNodesVar) (freeUserName commNodesVar) conn
      
      undefined
    
  
  return $ ServerEnvironment
    { serverCommNodes = commNodesVar
    , serverShutdown = shutdownVar
    }

serverLoop :: TVar [User] -> TVar Bool -> TVar [ServerCommNode] -> IO ()
serverLoop = undefined

toUserList :: TVar [ServerCommNode] -> STM [User]
toUserList commNodesVar = do
  commNodes <- readTVar commNodesVar
  return $ fmap commNodeUserName commNodes

lockUserName :: TVar [ServerCommNode] -> User -> STM Bool
lockUserName commNodesVar user = do
  userlist <- toUserList commNodesVar
  if user `elem` userlist then do
    return False
  else do
    writeTVar userlistVar (user : userlist)
    return True
  
freeUserName :: TVar [ServerCommNode] -> User -> STM ()
freeUserName commNodesVar user = modifyTVar (filter ((/= user) . commNodeUserName)) commNodesVar

stm :: STM a -> Session s s a
stm = io . atomically

isTerminated :: ServerCommNode -> STM Bool
isTerminated = readTVar . commNodeTerminated

receiveCommand :: ServerCommNode -> IO (Maybe ClientCommand)
receiveCommand commNode = undefined

writeCommand :: ServerCommNode -> ServerCommand -> IO ()
writeCommand commNode command = undefined

spawnServerCommNode :: (User -> STM Bool) -> (User -> STM ()) -> Connection -> IO ServerCommNode
spawnServerCommNode lockUserName freeUserName conn = do
  
  userNameVar <- newEmptyTMVarIO
  sendCommandVar <- newEmptyTMVarIO
  recvCommandVar <- newEmptyTMVarIO
  terminateVar <- newTVarIO False
  
  _iThreadId <- forkIO $ accept (serverInputConn conn) 
                       $ inputThread terminateVar recvCommandVar userNameVar
  
  _oThreadId <- forkIO $ accept (serverOutputConn conn) 
                       $ outputThread terminateVar sendCommandVar
  
  userName <- atomically $ readTMVar userNameVar
  
  return $ ServerCommNode
    { commNodeSendCommand = sendCommandVar
    , commNodeRecvCommand = recvCommandVar
    , commNodeTerminated = terminateVar
    , commNodeUserName = userName
    }
  where
    inputThread :: TVar Bool -> TMVar ClientCommand -> TMVar User -> Session (Cap () ServerInput) () ()
    inputThread terminateVar recvCommandVar userNameVar = do
      userName <- recv
      stm $ let -- TODO: Nested do block
              (>>=) = (Prelude.>>=)
        in do
          validUserName <- lockUserName userName
          unless validUserName $ writeTVar terminateVar True
          putTMVar userNameVar userName
      enter -- Enter the recursive loop
      inputThreadLoop userName terminateVar recvCommandVar
      where -- TODO: Bind annotations
        (>>=) :: Session i j a -> (a -> Session j k b) -> Session i k b
        (>>=) = (>>>=)
        (>>) :: Session i j a -> Session j k b -> Session i k b
        a >> b = a >>= const b
        return :: a -> Session i i a
        return = ireturn
        fail :: String -> Session i j a
        fail = error
    
    inputThreadLoop :: User -> TVar Bool -> TMVar ClientCommand
                    -> Session (Cap ((ServerInputLoop (Var Z)), ()) (ServerInputLoop (Var Z))) () ()
    inputThreadLoop userName terminateVar recvCommandVar = do
      terminate <- stm $ readTVar terminateVar
      if terminate then do
        sel1
        close
      else do
        sel2
        command <- recv
        stm $ -- TODO: Nested do block
          when (command == EndSession) (writeTVar terminateVar True Prelude.>> freeUserName userName)
          Prelude.>> putTMVar recvCommandVar command
        zero
        inputThreadLoop userName terminateVar recvCommandVar
      where -- TODO: Bind annotations
        (>>=) :: Session i j a -> (a -> Session j k b) -> Session i k b
        (>>=) = (>>>=)
        (>>) :: Session i j a -> Session j k b -> Session i k b
        a >> b = a >>= const b
        return :: a -> Session i i a
        return = ireturn
        fail :: String -> Session i j a
        fail = error
    
    outputThread :: TVar Bool -> TMVar ServerCommand -> Session (Cap () ServerOutput) () ()
    outputThread terminateVar sendCommandVar = do
      enter
      outputThreadLoop terminateVar sendCommandVar
      where -- TODO: Bind annotations
        (>>=) :: Session i j a -> (a -> Session j k b) -> Session i k b
        (>>=) = (>>>=)
        (>>) :: Session i j a -> Session j k b -> Session i k b
        a >> b = a >>= const b
        return :: a -> Session i i a
        return = ireturn
    
    outputThreadLoop :: TVar Bool -> TMVar ServerCommand
                     -> Session (Cap ((ServerOutputLoop (Var Z)), ()) (ServerOutputLoop (Var Z))) () ()
    outputThreadLoop terminateVar sendCommandVar = do
      terminate <- stm $ readTVar terminateVar
      if terminate then do
        sel1
        close
      else do
        sel2
        command <- stm $ takeTMVar sendCommandVar
        send command
        zero
        outputThreadLoop terminateVar sendCommandVar
      where -- TODO: Bind annotations
        (>>=) :: Session i j a -> (a -> Session j k b) -> Session i k b
        (>>=) = (>>>=)
        (>>) :: Session i j a -> Session j k b -> Session i k b
        a >> b = a >>= const b
        return :: a -> Session i i a
        return = ireturn
        fail :: String -> Session i j a
        fail = error







    