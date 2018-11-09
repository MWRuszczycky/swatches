{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( squareUI
    , sortedUI
    , usage
    , theMap
    ) where

import Data.List            ( intersperse
                            , intercalate )
import Types                ( Name (..) )
import Brick.Widgets.Center ( center
                            , hCenter )
import Graphics.Vty         ( Color (..)
                            , withStyle
                            , bold
                            , white
                            , black
                            , defAttr )
import Brick                ( Widget
                            , ViewportType (..)
                            , AttrMap
                            , on
                            , str
                            , withAttr
                            , hBox
                            , vBox
                            , hLimit
                            , fill
                            , vLimit
                            , viewport
                            , cropToContext
                            , attrMap
                            , attrName )

---------------------------------------------------------------------
-- UI renderer

squareUI :: s -> [Widget Name]
-- ^Take the current program state and generate a list of widgets.
-- Currently is independent of state and just draws rows of labels
-- and color swatches.
squareUI = const [ uiScroll ]
    where ui       = vBox . shuffleIn labels $ swatches
          rowVals  = [ [ 16 * r + c | c <- [0..15] ] | r <- [0..15] ]
          swatches = map ( hBox . swatchSeries 5 1 ) rowVals
          labels   = map ( hBox . labelSeries 5 1 ) rowVals
          uiScroll = viewport Swatches Both $ ui

sortedUI :: s -> [Widget Name]
sortedUI = const [ uiScroll ]
    where rgbVals  = [ [ c * 36 + r + 16 | r <- [0..35] ] | c <- [0..5] ]
          rgbSwts  = map ( vBox . swatchSeries 5 1 ) rgbVals
          rgbLbls  = map ( vBox . labelSeries 5 1 ) rgbVals
          rgb      = hBox . shuffleIn rgbLbls $ rgbSwts
          ansVals  = [0..15]
          ansSwts  = vBox . swatchSeries 5 1 $ ansVals
          ansLbls  = vBox . labelSeries 5 1 $ ansVals
          ans      = hBox [ ansLbls, ansSwts ]
          greyVals = [232..255]
          greySwts = vBox . swatchSeries 5 1 $ greyVals
          greyLbls = vBox . labelSeries 5 1 $ greyVals
          greyScl  = hBox [ greyLbls, greySwts ]
          uiScroll = viewport Swatches Both . hBox $ [rgb, greyScl, ans]

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

swatchSeries :: Int -> Int -> [Int] -> [Widget Name]
-- ^List of swatch widgets constructed from a list of color values.
-- Each swatch has the width cw and they are separated by sw spaces.
swatchSeries cw sw vs = intersperse sep $ clrs
    where clrs = map ( \ x -> withAttr ( attrName . show $ x ) area ) vs
          area = swatch cw
          sep  = separator sw

labelSeries :: Int -> Int -> [Int] -> [Widget Name]
-- ^List of numbers used to annotate the swatches where each number
-- is centered in a horizontal space of a fixed width nw and is
-- separated from the adjacent number by sw spaces.
labelSeries nw sw ns = intersperse sep $ nums
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
-- Usage information

usage :: IO ()
usage = putStrLn . unlines $ helpStr
    where helpStr = [ "Swatches"
                    , "Displays the 256 colors available in the terminal."
                    , "   swatches           : display a 16 x 16 unsorted grid"
                    , "   swatches --stacked : display sorted colors" ]

---------------------------------------------------------------------
-- Helper functions

shuffleIn :: [a] -> [a] -> [a]
-- ^Evenly shuffles two lists together into a new list with the
-- combined values.
shuffleIn [] ys         = ys
shuffleIn xs []         = xs
shuffleIn (x:xs) (y:ys) = x:y:( shuffleIn xs ys )
