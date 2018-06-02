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

mngEvent :: () -> BrickEvent Name e -> EventM Name ( Next () )
-- ^Manages events, which are presently just for scrolling around in
-- the window. Any non-supported event ends the program. The
-- scrolling always works as expected with the arrow keys. Character
-- keys can also be used for scrolling; however, right now they are
-- are based on Dvorak setup that I use. This will be changed later.
mngEvent _ ( VtyEvent ( EvKey k [] ) ) =
    case k of
         KUp       -> vScrollBy vpScroll (-1) >> continue ()
         KDown     -> vScrollBy vpScroll 1    >> continue ()
         KRight    -> hScrollBy vpScroll 1    >> continue ()
         KLeft     -> hScrollBy vpScroll (-1) >> continue ()
         KChar 'k' -> vScrollBy vpScroll 1    >> continue ()
         KChar 'j' -> vScrollBy vpScroll (-1) >> continue ()
         KChar 'h' -> hScrollBy vpScroll (-1) >> continue ()
         KChar 't' -> hScrollBy vpScroll 1    >> continue ()
         otherwise -> halt ()
    where vpScroll = viewportScroll Swatches
mngEvent _ _ = halt ()
