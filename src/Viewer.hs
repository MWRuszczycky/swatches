{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( usage
    , routeView
    , theMap
    ) where

import Data.List            ( intersperse
                            , intercalate
                            , sortOn      )
import Types                ( Name (..) )
import Graphics.Vty         ( withBackColor
                            , black
                            , white
                            , defAttr )
import Brick                ( Widget
                            , ViewportType (..)
                            , AttrMap
                            , AttrName (..)
                            , (<+>)
                            , fg
                            , bg
                            , str
                            , withAttr
                            , vBox
                            , viewport
                            , attrMap
                            , attrName )
import Model                ( palette256
                            , palette240
                            , paletteGreys
                            , palette16
                            , hsvSort
                            , rgbToHSV )
import Types                ( Setup (..), RGB (..), Color (..) )

---------------------------------------------------------------------
-- UI renderer

routeView :: Setup -> [ Widget Name ]
routeView = spectrum

spectrum :: Setup -> [ Widget Name ]
spectrum st = [ viewport Swatches Both ui ]
    where uiLine c = swatch 3 c <+> separator 1 <+> swatchStr (testString st) c
          ui       = vBox . map uiLine . hsvSort $ palette256

---------------------------------------------------------------------
-- M.Named widgets

separator :: Int -> Widget Name
-- ^Spacing widget with a given width used to separate swatches.
separator w = withAttr "spacer" . str . replicate w $ ' '

swatchStr :: String -> Color -> Widget Name
swatchStr s c = withAttr ( colorFG c ) . str $ s

swatch :: Int -> Color -> Widget Name
swatch w c = withAttr ( colorBG c ) . str . replicate w $ ' '

colorFG :: Color -> AttrName
colorFG = attrName . ('f':) . show . rgb

colorBG :: Color -> AttrName
colorBG = attrName . ('b':) . show . rgb

---------------------------------------------------------------------
-- Attribute map

theMap :: AttrMap
theMap = attrMap defAttr . concat $
            [ -- foreground color map: prefix hexcode with 'f'
              [ (attrName . ('f':) . show . rgb $ c, fg . color $ c)
                    | c <- palette256 ]
              -- background color map: prefix hexcode with 'b'
            , [ (attrName . ('b':) . show . rgb $ c, bg . color $ c)
                    | c <- palette256 ]
            ]

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
