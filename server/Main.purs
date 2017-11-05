module Main where

import Prelude (Unit, pure, discard, bind, unit, void, ($))
import Data.Maybe (Maybe(Nothing))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.HTTP (Server, HTTP, listen, createServer, setStatusCode, responseAsStream)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (end, writeString)

foreign import data SocketIO :: Type
foreign import data SOCKET_IO :: Effect
foreign import getSocketIOImpl :: forall e. Server -> Eff (socket_io :: SOCKET_IO | e) SocketIO
foreign import ioOn :: forall e1 e. SocketIO -> String -> (SocketIO -> Eff e1 Unit) -> Eff (socket_io :: SOCKET_IO | e) Unit
foreign import ioEmit :: forall o e. SocketIO -> String -> o -> Eff (socket_io :: SOCKET_IO | e) Unit
foreign import socketEmit :: forall o e. SocketIO -> String -> o -> Eff (socket_io :: SOCKET_IO | e) Unit
foreign import socketOn :: forall e e1 a. SocketIO -> String -> (a -> Eff e1 Unit) -> Eff (socket_io :: SOCKET_IO | e) Unit

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, socket_io :: SOCKET_IO | e) Unit
main = do
  server <- createServer handler
  io <- getSocketIOImpl server

  ioOn io "connection" \socket -> do
    log "IO Connection"

    socketOn socket "answer" \d -> do
      log "Got an answer"
      log d
      ioEmit io "new answer" d

    socketOn socket "disconnect" \d -> do
      log "Disconnected"

  listen server { hostname: "purescript-presentation.local", port: 8080, backlog: Nothing } $ void do
    log "Listening on port 8080"


  where
    handler req res = do
      setStatusCode res 200
      let outputStream = responseAsStream res
      _ <- writeString outputStream UTF8 "Foo" (pure unit)
      end outputStream (pure unit)