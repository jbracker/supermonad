
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Types 
  ( Message, User
  , ClientCommand(..), ServerCommand(..)
  , ServerInput, ServerOutput
  , ServerInputLoop, ServerOutputLoop
  ) where 

import Control.Concurrent.SimpleSession.SessionTypes 
  ( Eps, Var, Rec, (:&:), (:+:), (:!:), (:?:)
  , Z )  

data ClientCommand 
  = ShutdownServer
  | SendMessage String
  | EndSession
  | DoNothing
  deriving ( Show, Eq )

data ServerCommand
  = NewMessage User Message
  | ClientLeft User
  deriving ( Show, Eq )

-- | Type of the user identifier.
type User = String

-- | Type of messages.
type Message = String

-- | End of communication.
type EndSession = Eps

-- | Intial handshake with the server. Send username and see if the server accepts it.
type ClientOutputInit r = User :!: r
type ClientInputInit  r = r

-- | Intial handshake from the servers perspective.
type ServerInputInit  r = User :?: r
type ServerOutputInit r = r

-- | The input channel of the client is constantly receiving new messages from the server,
--   or the server is shutdown and communication is ended.
type ClientInputLoop r = EndSession :&: (ServerCommand :?: r)
-- | The output channel of the client first receives notice of the server is shutting down.
--   Afterwards the client can choose to send messages, get the list of users or shutdown the server.
type ClientOutputLoop r = EndSession :&: (ClientCommand :!: r)

type ServerInputLoop  r = EndSession :+: (ClientCommand :?: r)
type ServerOutputLoop r = EndSession :+: (ServerCommand :!: r)


type ServerInput  = ServerInputInit  (Rec (ServerInputLoop  (Var Z)))
type ServerOutput = ServerOutputInit (Rec (ServerOutputLoop (Var Z)))



