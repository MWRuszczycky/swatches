{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( routeView
    , makeMap
    ) where

import qualified Types                as T
import qualified Graphics.Vty         as Vty
import qualified Brick                as B
import qualified Brick.Widgets.Center as B
import Data.List                             ( intersperse  )
import Brick                                 ( (<=>), (<+>) )
import Model                                 ( palette256
                                             , palette240
                                             , paletteGreys
                                             , palette16
                                             , sortPalette
                                             , value
                                             , rgbToHSV     )

---------------------------------------------------------------------
-- interfaces

routeView :: T.Setup -> [ B.Widget T.Name ]
routeView st = case T.mode st of
                    T.Block   -> blockUI st
                    otherwise -> spectrumUI st

spectrumUI :: T.Setup-> [ B.Widget T.Name ]
spectrumUI st = [ B.viewport T.Swatches B.Both $ title <=> ui ]
    where s     = T.string st
          go    = T.sortDir st . sortPalette (T.sortCode st)
          ui    = B.vBox . map (specLine s) . go $ palette256
          title = B.withAttr "label" . B.hLimit w . B.hCenter . B.str $ note
          note  = "hexcodes may be incorrect for user-defined colors"
          w     | length note > 25 + length s = length note
                | otherwise                   = 25 + length s

blockUI :: T.Setup -> [ B.Widget T.Name ]
blockUI st = [ B.viewport T.Swatches B.Both ( ui16 <=> ui240 ) ]
    where go    = T.sortDir st . sortPalette (T.sortCode st)
          ui16  = labelRow palette16 <=> swatchRow palette16
          cs240 = breakInto 16 . go $ palette240
          ui240 = B.vBox [ labelRow cs <=> swatchRow cs | cs <- cs240 ]

---------------------------------------------------------------------
-- widgets

label :: Int -> (T.Color -> String) -> T.Color -> B.Widget T.Name
label w f = B.withAttr "label" . B.hLimit w . B.hCenter . B.str . f

labelRow :: T.Palette -> B.Widget T.Name
labelRow = B.hBox . intersperse ( hSeparator 1 ) . map ( label 5 go )
    where go = show . T.code

swatch :: Int -> T.Color -> B.Widget T.Name
-- ^horizontal color swatch of given width.
swatch w c = B.withAttr ( colorBG c ) . B.str . replicate w $ ' '

coloredString :: String -> T.Color -> B.Widget T.Name
-- ^Colored string on the default background.
coloredString s c = B.withAttr ( colorFG c ) . B.str $ s

swatchRow :: T.Palette -> B.Widget T.Name
swatchRow = B.hBox . intersperse ( hSeparator 1 ) . map ( swatch 5 )

hSeparator :: Int -> B.Widget T.Name
-- ^Horizontal using default colors.
hSeparator w = B.str . replicate w $ ' '

specLine :: String -> T.Color -> B.Widget T.Name
specLine s c = B.hBox [ label 8 (show . T.rgb) c
                      , hSeparator 1
                      , label 3 (show . T.code) c
                      , hSeparator 1
                      , swatch 3 c
                      , hSeparator 3
                      , coloredString s c
                      , hSeparator 3
                      , swatch 3 c
                      ]

---------------------------------------------------------------------
-- Attributes

colorFG :: T.Color -> B.AttrName
colorFG = B.attrName . ('f':) . show . T.rgb

colorBG :: T.Color -> B.AttrName
colorBG = B.attrName . ('b':) . show . T.rgb

getVtyColor :: T.ColorCode -> Maybe Vty.Color
getVtyColor c
    | c < 0     = Nothing
    | c < 16    = Just . Vty.ISOColor . fromIntegral $ c
    | c < 256   = Just . Vty.Color240 . fromIntegral $ c - 16
    | otherwise = Nothing

makeMap :: T.Setup -> B.AttrMap
makeMap st = let clr    = T.background st >>= getVtyColor
                 dattr  = maybe Vty.defAttr (Vty.withBackColor Vty.defAttr) clr
                 fgname = B.attrName . ('f':) . show . T.rgb
                 bgname = B.attrName . ('b':) . show . T.rgb
             in  B.attrMap dattr . concat $
                     [ -- foreground color map: prefix hexcode with 'f'
                       [ (fgname c, B.fg . T.color $ c) | c <- palette256 ]
                       -- background color map: prefix hexcode with 'b'
                     , [ (bgname c, B.bg . T.color $ c) | c <- palette256 ]
                       -- base attributes
                     , [ ("label", Vty.withStyle Vty.defAttr Vty.bold) ]
                     ]

---------------------------------------------------------------------
-- Helper functions

breakInto :: Int -> [a] -> [[a]]
breakInto _ [] = []
breakInto n xs = ps : breakInto n ss
    where (ps, ss) = splitAt n xs
