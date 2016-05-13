
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Types 
  ( Message, User
  , Update(..), Request(..), Response(..)
  , UpdateS, RequestS, EndS
  , ServerInit, ServerProtocol
  , ClientInit, ClientProtocol
  , Connection(..), mkConnection
  ) where 

import Control.Supermonad.Prelude

import Control.Concurrent.SimpleSession.SessionTypes 
  ( Eps, Var, Rec, (:&:), (:+:), (:!:), (:?:)
  , Z )
import Control.Concurrent.SimpleSession.Implicit 
  ( Session
  , Rendezvous, newRendezvous )

import Control.Monad.Indexed ( (>>>=), ireturn, imap )

instance Functor (Session i j) where
  fmap = imap

instance Bind (Session i j) (Session j k) (Session i k) where
  (>>=) = (>>>=)

instance Return (Session i i) where
  return = ireturn

instance Fail (Session i j) where
  fail = error

-- | Type of the user identifier.
type User = String

-- | Type of messages.
type Message = String

-- | Updates from the server.
data Update 
  = UserLeftChat User
  | NewMessage User Message
  | NoUpdate
  deriving ( Eq, Show )

-- | Requests from the client.
data Request
  = SendMessage Message
  | ShutdownServer
  | FetchUserList
  | NoRequest
  deriving ( Eq, Show )

-- | Responses from the server to the client 'Request's.
data Response
  = EmptyResponse
  | UserListResponse [User]
  deriving ( Eq, Show )

-- | Type of a connection between server and client (View from server-side).
data Connection = Connection { unwrapConnection :: Rendezvous (ServerInit (ServerProtocol (Var Z))) }

-- | Create a new connection.
mkConnection :: IO Connection
mkConnection = Connection <$> newRendezvous  
  
-- | The server send the most current 'Update's to the client.
type UpdateS r = [Update] :!: r

-- | The client sends a 'Request' and receives a 'Response' from the server.
type RequestS r = Request :?: (Response :!: r)

-- | The server and the client have a choice to end communication...
type EndS r = Eps :+: (Eps :&: r)

-- | Initially the client has to introduce itself by name.
type ServerInit r = User :?: (Rec r)

-- | After the introduction we repeat the steps: 
--   1. End of communication? 2. Updates from server; 3. Requests from the client;
type ServerProtocol r = EndS (UpdateS (RequestS r))

type EndC r = Eps :&: (Eps :+: r)
type RequestC r = Request :!: (Response :?: r)
type UpdateC r = [Update] :?: r

-- | The client-side version of the 'ServerInit'.
type ClientInit r = User :!: (Rec r)

-- | The client-side version of the 'ServerProtocol'.
type ClientProtocol r = EndC (UpdateC (RequestC r))




