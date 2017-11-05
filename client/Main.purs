module Main where

import Prelude (Unit, pure, bind, unit, map, ($), (<$>), (=<<))
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlElementToElement, htmlDocumentToDocument)
import DOM.HTML.Window (document, alert)
import DOM.HTML.Document (body)
import DOM.Node.Document (getElementsByTagName)
import DOM.Event.EventTarget (EventListener, eventListener)
import FRP (FRP)
import FRP.Behavior (animate)
import Client.Socket (SOCKET_IO_CLIENT, setupSocket, socketEmit)
import Perusal.HTML (toSlides, render)
import Perusal.Navigation (movement)
import Text.Smolder.HTML (div, button, textarea)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, text, on, (!), (#!))
import Text.Smolder.Renderer.DOM (render) as R

-- | Setup the socket, Get the slides, attach the controls, run the thingy!
main :: forall eff
      . Eff (socket_io_client :: SOCKET_IO_CLIENT, alert :: ALERT, dom :: DOM, frp :: FRP | eff) Unit
main = do
  socket    <- setupSocket
  window'   <- window
  document' <- document window'
  tape      <- toSlides =<< getElementsByTagName "section" (htmlDocumentToDocument document')
  mbody     <- map htmlElementToElement <$> body document'

  _ <- let
          sendMsgToServer msg d = \_ -> socketEmit socket msg d

          clickListener = eventListener (sendMsgToServer "answer" "Foo")

          markup :: forall e. Markup (EventListener (socket_io_client :: SOCKET_IO_CLIENT | e))
          markup = div ! className "user-input" $ do
            textarea ! className "user-input--answer" $ text ""
            button #! on "click" (clickListener) $ text "Do Stuff!"
        in
          case mbody of
            Just x -> R.render x markup
            Nothing -> pure unit

  case tape of
    Just slides -> animate (movement slides) render
    Nothing -> alert "Where are your slides?" window'