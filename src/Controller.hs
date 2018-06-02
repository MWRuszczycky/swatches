{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( mngEvent
    ) where

import Types          ( Name (..) )
import Brick.Types    ( BrickEvent (..)
                      , EventM
                      , Next )
import Brick.Main     ( ViewportScroll
                      , viewportScroll
                      , vScrollBy
                      , hScrollBy
                      , halt
                      , continue )
import Graphics.Vty   ( Event (..)
                      , Key (..) )

vpScroll :: ViewportScroll Name
vpScroll = viewportScroll Swatches

mngEvent :: () -> BrickEvent Name e -> EventM Name ( Next () )
mngEvent _ ( VtyEvent ( EvKey k [] ) ) =
    case k of
         KUp     -> vScrollBy vpScroll (-1) >> continue ()
         KDown   -> vScrollBy vpScroll 1    >> continue ()
         KRight  -> hScrollBy vpScroll 1    >> continue ()
         KLeft   -> hScrollBy vpScroll (-1) >> continue ()
         otherwise -> halt ()
mngEvent _ _ = halt ()
