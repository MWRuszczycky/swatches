{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( routeEvent
    ) where

import qualified Brick        as B
import qualified Graphics.Vty as Vty
import qualified Types        as T
import Model                         ( moveZneg
                                     , moveZpos
                                     , rotXneg
                                     , rotXpos
                                     , rotZneg
                                     , rotZpos  )

-- =============================================================== --
-- Helper types and main router

type EventRoute e = B.BrickEvent T.Name e -> B.EventM T.Name ( B.Next T.Setup )
type KeyEventRoute e = Vty.Key -> [Vty.Modifier] -> EventRoute e

routeEvent :: T.Setup -> EventRoute e
routeEvent st = case T.mode st of
                     T.Cube c  -> cubeEvent c st
                     otherwise -> scrollEvent st

-- =============================================================== --
-- Event managers for the cube-mode interface

cubeEvent :: T.RGBCube -> T.Setup -> EventRoute e
-- ^Handle events for the cube mode.
cubeEvent c st ( B.VtyEvent ( Vty.EvKey k ms ) )
    | elem k ctrls = B.continue $ st { T.mode = T.Cube (moveCube k ms c) }
    | otherwise    = B.halt st
    where ctrls = [ Vty.KDown, Vty.KUp, Vty.KLeft, Vty.KRight ]
cubeEvent _ st _ = B.halt st

moveCube :: Vty.Key -> [Vty.Modifier] -> T.RGBCube -> T.RGBCube
-- ^Rotate and move the cube according to keyboard input.
moveCube Vty.KDown  []             = moveZneg
moveCube Vty.KUp    []             = moveZpos
moveCube Vty.KRight []             = rotZpos
moveCube Vty.KLeft  []             = rotZneg
moveCube Vty.KDown (Vty.MShift:[]) = rotXpos
moveCube Vty.KUp   (Vty.MShift:[]) = rotXneg
moveCube _          _              = id

-- =============================================================== --
-- Event managers for scrollable interfaces.

scrollEvent :: T.Setup -> EventRoute e
-- ^Scrolling always works as expected with the arrow keys. Character
-- keys can also be used for scrolling; however, right now they are
-- are based on the Dvorak keyboard. This will be changed later.
scrollEvent st ( B.VtyEvent ( Vty.EvKey k [] ) ) =
    case k of
         Vty.KUp       -> B.vScrollBy vpScroll (-1) >> B.continue st
         Vty.KDown     -> B.vScrollBy vpScroll 1    >> B.continue st
         Vty.KRight    -> B.hScrollBy vpScroll 1    >> B.continue st
         Vty.KLeft     -> B.hScrollBy vpScroll (-1) >> B.continue st
         Vty.KChar 'k' -> B.vScrollBy vpScroll (-1) >> B.continue st
         Vty.KChar 'j' -> B.vScrollBy vpScroll 1    >> B.continue st
         Vty.KChar 'h' -> B.hScrollBy vpScroll (-1) >> B.continue st
         Vty.KChar 't' -> B.hScrollBy vpScroll 1    >> B.continue st
         otherwise -> B.halt st
    where vpScroll = B.viewportScroll T.Swatches
scrollEvent st _ = B.halt st
