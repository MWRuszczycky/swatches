{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( drawUI
    , theMap
    ) where

import Data.List            ( intersperse )
import Brick.Util           ( on )
import Types                ( Name (..) )
import Brick.Types          ( Widget
                            , ViewportType (..) )
import Brick.Widgets.Center ( center
                            , hCenter)
import Brick.Widgets.Core   ( str
                            , withAttr
                            , hBox
                            , vBox
                            , hLimit
                            , fill
                            , vLimit
                            , viewport
                            , cropToContext )
import Brick.AttrMap        ( AttrMap
                            , attrMap
                            , attrName )
import Graphics.Vty         ( Color (..)
                            , withStyle
                            , bold
                            , white
                            , black
                            , defAttr )

---------------------------------------------------------------------
-- UI renderer

drawUI :: s -> [Widget Name]
-- ^Take the current program state and generate a list of widgets.
-- Currently is independent of state and just draws rows of labels
-- and color swatches.
drawUI = const [ uiScroll ]
    where ui       = vBox . shuffleIn labels $ swatches
          rowVals  = [ [ 16 * r + c | c <- [0..15] ] | r <- [0..15] ]
          swatches = map ( rowOfSwatches 5 1 ) rowVals
          labels    = map ( rowOfLabels 5 1 ) rowVals
          uiScroll = viewport Swatches Both $ ui

---------------------------------------------------------------------
-- M.Named widgets

separator :: Int -> Widget Name
-- ^Spacing widget with a given width used to separate swatches.
separator w = withAttr "spacer" . str . replicate w $ ' '

swatch :: Int -> Widget Name
-- ^Reserved line area that will be filled with a color having a
-- given width.
swatch w = str . replicate w $ ' '

---------------------------------------------------------------------
-- Widget contructors

rowOfSwatches :: Int -> Int -> [Int] -> Widget Name
-- ^Row of swatches constructed from a list of color values. Each
-- swatch has the width cw and they are separated by sw spaces.
rowOfSwatches cw sw vs = hBox . intersperse sep $ clrs
    where clrs = map ( \ x -> withAttr ( attrName . show $ x ) area ) vs
          area = swatch cw
          sep  = separator sw

rowOfLabels :: Int -> Int -> [Int] -> Widget Name
-- ^Row of numbers used to annotate the swatches where each number
-- is centered in a horizontal space of a fixed width nw and is
-- separated from the adjacent number by sw spaces.
rowOfLabels nw sw ns = hBox . intersperse sep $ nums
    where fmt  = hLimit nw . hCenter . str . show
          nums = map ( withAttr "label" . fmt ) ns
          sep  = separator sw

---------------------------------------------------------------------
-- Attribute map

theMap :: AttrMap
-- ^Attribute map. This is divided into two main parts. The first are
-- all the color values, which are divided into the 8 ANSI colors and
-- their 8 bold versions, which take values 0--15, and the full RGB
-- set that ranges from 16--255. The greyscale colors are in the
-- range 232--255. Thes attributes are used to color on-screen
-- swatches. The second part of the map contains attributes for color
-- labels and separators between the colored swatches.
theMap = attrMap defAttr $ clrsANSI ++ clrsRGB ++ clrsDisp
    where clrsRGB  = [ ( attrName . show $ n
                       , black `on` Color240 (n - 16) ) | n <- [16..255] ]
          clrsANSI = [ ( attrName . show $ n
                       , black `on` ISOColor n ) | n <- [0..15] ]
          clrsDisp = [ ( "spacer", black `on` black )
                     , ( "label", withStyle ( white `on` black ) bold ) ]

---------------------------------------------------------------------
-- Helper functions

shuffleIn :: [a] -> [a] -> [a]
-- ^Evenly shuffles two lists together into a new list with the
-- combined values.
shuffleIn [] ys         = ys
shuffleIn xs []         = xs
shuffleIn (x:xs) (y:ys) = x:y:( shuffleIn xs ys )
