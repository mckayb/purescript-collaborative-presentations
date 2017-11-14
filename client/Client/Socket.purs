module Client.Socket where

import Prelude (Unit, discard, bind, pure)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (log)

foreign import data SocketIOClient :: Type
foreign import data SOCKET_IO_CLIENT :: Effect
foreign import getSocketIOClientImpl :: forall e. String -> Eff (socket_io_client :: SOCKET_IO_CLIENT | e) SocketIOClient
foreign import socketOn :: forall e e1 a. SocketIOClient -> String -> (a -> Eff e1 Unit) -> Eff (socket_io_client :: SOCKET_IO_CLIENT | e) Unit
foreign import socketEmit :: forall o e. SocketIOClient -> String -> o -> Eff (socket_io_client :: SOCKET_IO_CLIENT | e) Unit

setupSocket :: forall a e1 e. (a -> Eff e1 Unit) -> Eff (socket_io_client :: SOCKET_IO_CLIENT | e) SocketIOClient
setupSocket onNewAnswer = do
  socket <- getSocketIOClientImpl "http://homestead.app:8080"

  socketOn socket "connect" \d -> do
    log "ON connect"
    log d

  socketOn socket "new answer" onNewAnswer

  pure socket
