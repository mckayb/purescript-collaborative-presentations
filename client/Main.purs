module Main where

import Prelude (Unit, pure, discard, bind, unit, map, ($), (<$>), (=<<), (<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
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
import Text.Smolder.Markup (Markup, text, on, (!), (#!))
import Text.Smolder.Renderer.DOM (render) as R
import Control.Monad.Eff.JQuery (JQuery, create, body, ready, select, toArray, getValue, setText)

foreign import trace :: forall a. a -> a
foreign import getElemValue :: forall e. Element -> Eff (dom :: DOM | e) String
foreign import jQueryToElement :: JQuery -> Element
foreign import after :: forall e. JQuery -> JQuery -> Eff (dom :: DOM | e) Unit

-- | Setup the socket, Get the slides, attach the controls, run the thingy!
-- main :: forall eff
      -- . Eff (socket_io_client :: SOCKET_IO_CLIENT, alert :: ALERT, dom :: DOM, frp :: FRP | eff) Unit

main = ready $ do
  slides <- select "section.slide.slide--original"
  arrSlides <- (map fromArray <<< toArray) slides
  b <- body
  case arrSlides of
    Just slides -> do
      socket <- setupSocket ( \a -> do
                                slide <- create "<section>"
                                setText a slide
                                visibleSlide <- select ".slide:visible"
                                after visibleSlide slide
                                -- TODO: Find a better way to get the slides added into the tape
                                -- let newSlides = add slides slide
                                -- let _  = trace newSlides
                                -- animate (movement (map jQueryToElement newSlides)) render
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
                button #! on "click" (clickListener) $ text "Do Stuff!"
            in R.render (jQueryToElement b) markup

      animate (movement (map jQueryToElement slides)) render
    Nothing -> do
      log "No Slides!"