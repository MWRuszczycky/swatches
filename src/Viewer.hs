{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( usage
    , routeView
    , theMap
    ) where

import Data.List            ( intersperse
                            , intercalate )
import Types                ( Name (..) )
import Brick.Widgets.Center ( center
                            , hCenter )
import Graphics.Vty         ( withStyle
                            , bold
                            , white
                            , black
                            , defAttr )
import Brick                ( Widget
                            , ViewportType (..)
                            , AttrMap
                            , (<+>)
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
import Model                ( palette256
                            , palette240
                            , paletteGreys
                            , palette16    )
import Types                ( Setup (..), RGB (..), Color (..) )

---------------------------------------------------------------------
-- UI renderer

routeView :: Setup -> [ Widget Name ]
routeView = spectrum

spectrum :: Setup -> [ Widget Name ]
spectrum st = [ vBox . map ( \ c -> swt c <+> tstr c ) $ palette256 ]
    where tstr x = withAttr (attrName . show . rgb $ x) . str . testString $ st
          swt  x = withAttr (attrName . show . rgb $ x) . swatch $ 3

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
theMap = attrMap defAttr [ (name x, attr x) | x <- palette256 ]
    where name = attrName . show . rgb
          attr = on black . color

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
