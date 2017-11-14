module Main where

import Prelude (Unit, pure, discard, bind, unit, map, ($), (<$>), (=<<), (==), (<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Data.Maybe (Maybe(..))
import Data.Tape (add, fromArray)
import DOM (DOM)
import DOM.HTML (window)
-- import DOM.HTML.Types (ALERT, htmlElementToElement, htmlDocumentToDocument)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (document, alert)
-- import DOM.HTML.Document (body)
import DOM.Node.Types (Element, ElementId(ElementId), documentToNonElementParentNode)
import DOM.Node.Document (getElementsByTagName)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Event.Types (Event)
import DOM.Event.EventTarget (EventListener, eventListener)
import FRP (FRP)
import FRP.Behavior (animate)
import Client.Socket (SOCKET_IO_CLIENT, setupSocket, socketEmit)
import Perusal.HTML (toSlides, render)
import Perusal.Navigation (movement)
import Text.Smolder.HTML (div, button, textarea)
import Text.Smolder.HTML.Attributes (id, className)
import Text.Smolder.Markup (Markup, text, (!), (#!))
import Text.Smolder.Markup (on) as M
import Text.Smolder.Renderer.DOM (render) as R
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, create, body, ready, select, toArray, getValue, setText, on, hide)

foreign import trace :: forall a. a -> a
foreign import getElemValue :: forall e. Element -> Eff (dom :: DOM | e) String
foreign import jQueryToElement :: JQuery -> Element
foreign import after :: forall e. JQuery -> JQuery -> Eff (dom :: DOM | e) Unit
foreign import getKey :: forall e. JQueryEvent -> Eff (dom :: DOM | e) Int

-- | Setup the socket, Get the slides, attach the controls, run the thingy!
-- main :: forall eff
      -- . Eff (socket_io_client :: SOCKET_IO_CLIENT, alert :: ALERT, dom :: DOM, frp :: FRP | eff) Unit

onKeypress e _ = do
  key <- getKey e
  case key of
    37 -> log "Going Left"
    39 -> log "Going Right"
    _ -> pure unit

main = ready $ do
  logShow keys.left
  jquerySlides <- select "section.slide.slide--original"
  arrSlides <- (map fromArray <<< toArray) jquerySlides
  b <- body
  case arrSlides of
    Just slides -> do
                            -- Need to have this update the state
      socket <- setupSocket ( \a -> do
                                slide <- create "<section>"
                                setText a slide
                                hide slide
                                visibleSlide <- select ".slide:visible"
                                after visibleSlide slide
                                render (map jQueryToElement (add slides slide))
                            )
      _ <- let
              sendMsgToServer :: forall e. String -> (Event -> Eff (dom :: DOM, socket_io_client :: SOCKET_IO_CLIENT | e) Unit)
              sendMsgToServer msg = \_ -> do
                elem <- select "#user-input"
                str <- getValue elem
                socketEmit socket msg str

              clickListener = eventListener (sendMsgToServer "answer")

              markup :: forall e. Markup (EventListener (dom :: DOM, socket_io_client :: SOCKET_IO_CLIENT | e))
              markup = div ! className "user-input" $ do
                textarea ! id "user-input" ! className "user-input--answer" $ text ""
                button #! M.on "click" (clickListener) $ text "Do Stuff!"
            in R.render (jQueryToElement b) markup

      -- Need some kind of state to update in here
      on "keypress" onKeypress b

      render (map jQueryToElement slides)

      -- animate (movement (map jQueryToElement slides)) render
    Nothing -> do
      log "No Slides!"