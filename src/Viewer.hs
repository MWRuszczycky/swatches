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
spectrum st = [ viewport Swatches Both ui ]
    where uiLine c = swatch 3 c <+> separator 1 <+> swatchStr (testString st) c
          ui       = vBox . map uiLine $ palette256

---------------------------------------------------------------------
-- M.Named widgets

separator :: Int -> Widget Name
-- ^Spacing widget with a given width used to separate swatches.
separator w = withAttr "spacer" . str . replicate w $ ' '

swatchStr :: String -> Color -> Widget Name
swatchStr s c = withAttr ( attrName . show . rgb $ c ) . str $ s

swatch :: Int -> Color -> Widget Name
swatch w c = withAttr ( attrName . show . rgb $ c ) . str . replicate w $ ' '

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
