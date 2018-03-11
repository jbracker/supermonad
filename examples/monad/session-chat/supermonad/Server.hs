
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

module Server 
  ( server
  , ServerEnv
  ) where

import Control.Supermonad.Prelude
import Control.Supermonad.Functions ( forM_, unless, when )

import Data.List ( find )

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, STM )
import Control.Concurrent.STM.TMVar
  ( TMVar
  , newEmptyTMVarIO
  , takeTMVar, putTMVar )
import Control.Concurrent.STM.TVar 
  ( TVar
  , newTVar, newTVarIO
  , readTVar, writeTVar, modifyTVar, swapTVar )

import Control.Concurrent.SimpleSession.Implicit 
  ( Session, Cap
  , Var, Z
  , accept
  , recv, send
  , sel1, sel2, offer
  , close
  , enter, zero
  )

import Utility ( stm )
import Types 
  ( User
  , Update(..), Request(..), Response(..)
  , ServerInit, ServerProtocol
  , Connection(..)
  )


data ServerEnv = ServerEnv
  { serverCommNodes :: TVar [(ServerCommNode, TVar [Update])]
  , serverUserList :: TVar [User]
  , serverShutdown :: TVar Bool
  }

mkServerEnv :: IO ServerEnv
mkServerEnv = atomically $ do
  shutdownVar <- newTVar False
  commNodesVar <- newTVar []
  userListVar <- newTVar []
  return $ ServerEnv
    { serverCommNodes = commNodesVar
    , serverUserList = userListVar
    , serverShutdown = shutdownVar
    }

server :: [Connection] -> IO ServerEnv
server conns = do
  serverEnv <- mkServerEnv
  
  _serverThreadId <- forkIO $ do
    forM_ conns $ \conn -> do
      -- Spawn a new communication node for the given connection.
      commNode <- spawnServerCommNode (lockUserName serverEnv) 
                                      (updateHandler serverEnv) 
                                      (requestHandler serverEnv)
                                      (terminationHandler serverEnv)
                                      conn
      -- For every communication node check if it is active (handshake successful)
      -- and if so add it to the list of communication nodes.
      atomically $ do
        terminated <- hasTerminated commNode
        userList <- readTVar $ serverUserList serverEnv
        unless (terminated && not (commNodeUserName commNode `elem` userList)) $ do
          updateVar <- newTVar []
          modifyTVar (serverCommNodes serverEnv) ((commNode, updateVar) :)
          
    serverLoop serverEnv
    
  return serverEnv
  where
    updateHandler :: ServerEnv -> User -> TVar Bool -> STM [Update]
    updateHandler serverEnv user _terminateVar = do
      mCommNode <- getUserCommNode serverEnv user
      case mCommNode of
        Just (_commNode, updatesVar) -> swapTVar updatesVar []
        Nothing -> return []
    
    requestHandler :: ServerEnv -> User -> TVar Bool -> Request -> STM Response
    requestHandler serverEnv user _terminateVar req = case req of
      SendMessage msg -> do 
        broadcastUpdateFrom serverEnv (NewMessage user msg) user
        return EmptyResponse
      ShutdownServer -> do
        writeTVar (serverShutdown serverEnv) True
        return EmptyResponse
      FetchUserList -> UserListResponse <$> readTVar (serverUserList serverEnv)
      NoRequest -> return EmptyResponse
    
    terminationHandler :: ServerEnv -> User -> STM ()
    terminationHandler serverEnv user = do
      modifyTVar (serverUserList serverEnv) $ filter (user /=)
      modifyTVar (serverCommNodes serverEnv) $ filter $ (user /=) . commNodeUserName . fst
      broadcastUpdateFrom serverEnv (UserLeftChat user) user
    
    lockUserName :: ServerEnv -> User -> STM Bool
    lockUserName serverEnv user = do
      userlist <- readTVar $ serverUserList serverEnv
      if user `elem` userlist then do
        return False
      else do
        writeTVar (serverUserList serverEnv) (user : userlist)
        return True

        
-- This function is dangerous if there are a lot of nodes, because it will iteratve all of them and 
-- that can lead to live-locks.
broadcastUpdateFrom :: ServerEnv -> Update -> User -> STM ()
broadcastUpdateFrom serverEnv update user = do
  commNodes <- readTVar (serverCommNodes serverEnv)
  forM_ commNodes $ \(node, updatesVar) -> do
    unless (commNodeUserName node == user) $ do
      modifyTVar updatesVar (update :)

getUserCommNode :: ServerEnv -> User -> STM (Maybe (ServerCommNode, TVar [Update]))
getUserCommNode serverEnv user = do
  commNodes <- readTVar (serverCommNodes serverEnv)
  return $ find ((user ==) . commNodeUserName . fst) commNodes

serverLoop :: ServerEnv -> IO ()
serverLoop serverEnv = do
  atomically $ do
    shutdown <- readTVar (serverShutdown serverEnv)
    when shutdown $ do
      commNodes <- readTVar (serverCommNodes serverEnv)
      forM_ commNodes $ \(node, _updates) -> do
        writeTVar (commNodeTerminated node) True
  
  serverLoop serverEnv

data ServerCommNode = ServerCommNode 
  { commNodeTerminated :: TVar Bool
  , commNodeUserName :: User
  , commNodeUpdateHandler :: User -> TVar Bool -> STM [Update]
  , commNodeRequestHandler :: User -> TVar Bool -> Request -> STM Response
  , commNodeTerminationHandler :: User -> STM ()
  }

hasTerminated :: ServerCommNode -> STM Bool
hasTerminated = readTVar . commNodeTerminated

spawnServerCommNode :: (User -> STM Bool) 
                    -> (User -> TVar Bool -> STM [Update]) 
                    -> (User -> TVar Bool -> Request -> STM Response) 
                    -> (User -> STM ())
                    -> Connection
                    -> IO ServerCommNode
spawnServerCommNode lockUserName updateHandler requestHandler terminationHandler conn = do
  
  userNameVar <- newEmptyTMVarIO
  terminateVar <- newTVarIO False
  
  _commThreadId <- forkIO $ accept (unwrapConnection conn) 
                          $ commThread terminateVar userNameVar
  
  userName <- atomically $ takeTMVar userNameVar
  
  return $ ServerCommNode
    { commNodeUpdateHandler = updateHandler
    , commNodeRequestHandler = requestHandler
    , commNodeTerminationHandler = terminationHandler
    , commNodeTerminated = terminateVar
    , commNodeUserName = userName
    }
  where
    commThread :: TVar Bool -> TMVar User -> Session (Cap () (ServerInit (ServerProtocol (Var Z)))) () ()
    commThread terminateVar userNameVar = do
      userName <- recv
      stm $ do
        validUserName <- lockUserName userName
        unless validUserName $ writeTVar terminateVar True
        putTMVar userNameVar userName
      enter -- Enter the recursive loop
      commThreadLoop userName terminateVar
    
    commThreadLoop :: User 
                   -> TVar Bool
                   -> Session (Cap ((ServerProtocol (Var Z)), ()) (ServerProtocol (Var Z))) () ()
    commThreadLoop userName terminateVar = do
      -- Termination phase
      terminated <- stm $ readTVar terminateVar
      if terminated then do
        sel1
        terminate
      else do
        sel2
        offer terminate $ do
          -- Update phase
          update <- stm $ updateHandler userName terminateVar
          send update
          -- Request phase
          request <- recv
          response <- stm $ requestHandler userName terminateVar request
          send response
          -- Recursive step
          zero
          commThreadLoop userName terminateVar
      where
        terminate = do
          close
          stm $ do
            writeTVar terminateVar True
            terminationHandler userName






    