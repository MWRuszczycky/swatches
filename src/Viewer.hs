{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( usage
    , routeView
    , theMap
    ) where

import Data.List            ( intersperse
                            , intercalate
                            , sortOn            )
import Types                ( Name         (..) )
import Graphics.Vty         ( withBackColor
                            , withForeColor
                            , black
                            , white
                            , defAttr           )
import Brick.Widgets.Center ( hCenter           )
import Brick                ( Widget
                            , ViewportType (..)
                            , AttrMap
                            , AttrName     (..)
                            , (<+>), (<=>)
                            , fg, bg
                            , str
                            , hLimit
                            , vBox, hBox
                            , viewport
                            , withAttr
                            , withDefAttr
                            , attrMap
                            , attrName          )
import Model                ( palette256
                            , palette240
                            , paletteGreys
                            , palette16
                            , sortPalette
                            , value
                            , rgbToHSV          )
import Types                ( Mode         (..)
                            , Setup        (..)
                            , RGB          (..)
                            , SortCode     (..)
                            , Color        (..) )

---------------------------------------------------------------------
-- interfaces

routeView :: Setup -> [ Widget Name ]
routeView st = case mode st of
                    Spectrum s -> if compressed st
                                     then spectrumC s
                                     else spectrum (testString st) s
                    otherwise  -> spectrum (testString st) "ansi"

spectrum :: String -> SortCode -> [ Widget Name ]
spectrum s sc = [ viewport Swatches Both ui ]
    where ui240 = vBox . map (specLine240 s) . sortPalette sc $ palette240
          ui16  = vBox . map (specLine16 s) $ palette16
          ui    = ui16 <=> separator 1 <=> ui240

spectrumC :: SortCode -> [Widget Name]
spectrumC sc = [ viewport Swatches Both ui ]
    where ui = hBox [ label' 3 (show . code) c | c <- palette16 ]

---------------------------------------------------------------------
-- widgets

label' :: Int -> (Color -> String) -> Color -> Widget Name
label' w f c
    | v < 10    = withDefAttr "fwhite" x
    | otherwise = withDefAttr "fblack" x
    where x = withAttr (colorBG c) . hLimit w . hCenter . str . f $ c
          v = value . rgb $ c

label :: Int -> AttrName -> String -> Widget Name
label w a = withDefAttr "fwhite" . withAttr a . hLimit w . hCenter . str

separator :: Int -> Widget Name
-- ^Spacing widget with a given width used to separate swatches.
separator w = withAttr "spacer" . str . replicate w $ ' '

swatchStr :: String -> Color -> Widget Name
swatchStr s c = withAttr ( colorFG c ) . str $ s

swatch :: Int -> Color -> Widget Name
swatch w c = withAttr ( colorBG c ) . str . replicate w $ ' '

specLine240 :: String -> Color -> Widget Name
specLine240 s c = swatch 3 c
                  <+> separator 3
                  <+> swatchStr s c
                  <+> separator 3
                  <+> swatch 3 c
                  <+> separator 3
                  <+> label 8 "default" (show . rgb $ c)
                  <+> separator 1
                  <+> label 3  "default" (show . code $ c)

specLine16 :: String -> Color -> Widget Name
specLine16 s c = swatch 3 c
                 <+> separator 3
                 <+> swatchStr s c
                 <+> separator 3
                 <+> swatch 3 c
                 <+> separator 3
                 <+> label 9 "default" ( (++ "?") . show . rgb $ c )
                 <+> separator 1
                 <+> label 3 "default" (show . code $ c)

---------------------------------------------------------------------
-- Attributes

colorFG :: Color -> AttrName
colorFG = attrName . ('f':) . show . rgb

colorBG :: Color -> AttrName
colorBG = attrName . ('b':) . show . rgb

theMap :: AttrMap
theMap = attrMap defAttr . concat $
            [ -- foreground color map: prefix hexcode with 'f'
              [ (attrName . ('f':) . show . rgb $ c, fg . color $ c)
                    | c <- palette256 ]
              -- background color map: prefix hexcode with 'b'
            , [ (attrName . ('b':) . show . rgb $ c, bg . color $ c)
                    | c <- palette256 ]
              -- base attributes
            , [ ("fblack", fg black)
              , ("fwhite", fg white) ]
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
