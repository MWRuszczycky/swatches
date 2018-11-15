{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( routeEvent
    ) where

import Types          ( Name (..), Setup )
import Graphics.Vty   ( Event (..)
                      , Key (..) )
import Brick          ( BrickEvent (..)
                      , EventM
                      , Next
                      , ViewportScroll
                      , viewportScroll
                      , vScrollBy
                      , hScrollBy
                      , halt
                      , continue )

routeEvent :: Setup -> BrickEvent Name e -> EventM Name ( Next Setup )
-- ^Manages events, which are presently just for scrolling around in
-- the window. Any non-supported event ends the program. The
-- scrolling always works as expected with the arrow keys. Character
-- keys can also be used for scrolling; however, right now they are
-- are based on Dvorak setup that I use. This will be changed later.
routeEvent st ( VtyEvent ( EvKey k [] ) ) =
    case k of
         KUp       -> vScrollBy vpScroll (-1) >> continue st
         KDown     -> vScrollBy vpScroll 1    >> continue st
         KRight    -> hScrollBy vpScroll 1    >> continue st
         KLeft     -> hScrollBy vpScroll (-1) >> continue st
         KChar 'k' -> vScrollBy vpScroll (-1) >> continue st
         KChar 'j' -> vScrollBy vpScroll 1    >> continue st
         KChar 'h' -> hScrollBy vpScroll (-1) >> continue st
         KChar 't' -> hScrollBy vpScroll 1    >> continue st
         otherwise -> halt st
    where vpScroll = viewportScroll Swatches
routeEvent st _ = halt st
