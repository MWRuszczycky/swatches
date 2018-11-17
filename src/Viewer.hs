{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( routeView
    , theMap
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
                    T.Spectrum  -> spectrumUI (T.testString st) (T.sortCode st)
                    T.Block     -> blockUI (T.sortCode st)
                    otherwise   -> spectrumUI (T.testString st) "ansi"

spectrumUI :: String -> T.SortCode -> [ B.Widget T.Name ]
spectrumUI s sc = [ B.viewport T.Swatches B.Both $ title <=> ui ]
    where ui    = B.vBox . map (specLine s) . reverse . sortPalette sc
                  $ palette256
          title = B.withAttr "label" . B.hLimit w . B.hCenter . B.str $ note
          note  = "hexcodes may be incorrect for user-defined colors"
          w     | length note > 25 + length s = length note
                | otherwise                   = 25 + length s

blockUI :: T.SortCode -> [ B.Widget T.Name ]
blockUI sc = [ B.viewport T.Swatches B.Both ( ui16 <=> ui240 ) ]
    where ui16  = labelRow palette16 <=> swatchRow palette16
          ui240 = B.vBox [ labelRow cs <=> swatchRow cs
                           | cs <- breakInto 16 . sortPalette sc $ palette240 ]

---------------------------------------------------------------------
-- widgets

label :: Int -> (T.Color -> String) -> T.Color -> B.Widget T.Name
label w f = B.withAttr "label" . B.hLimit w . B.hCenter . B.str . f

labelRow :: T.Palette -> B.Widget T.Name
labelRow = B.hBox . intersperse (hSeparator 1) . map (label 5 (show . T.code))

swatch :: Int -> T.Color -> B.Widget T.Name
-- ^horizontal color swatch of given width.
swatch w c = B.withAttr ( colorBG c ) . B.str . replicate w $ ' '

coloredString :: String -> T.Color -> B.Widget T.Name
-- ^Colored string on the default background.
coloredString s c = B.withAttr ( colorFG c ) . B.str $ s

swatchRow :: T.Palette -> B.Widget T.Name
swatchRow = B.hBox . intersperse (hSeparator 1) . map (swatch 5)

hSeparator :: Int -> B.Widget T.Name
-- ^Horizontal using default colors.
hSeparator w = B.str . replicate w $ ' '

specLine :: String -> T.Color -> B.Widget T.Name
specLine s c = B.hBox [ label 8 ( show . T.rgb) c
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

theMap :: B.AttrMap
theMap = B.attrMap Vty.defAttr . concat $
            [ -- foreground color map: prefix hexcode with 'f'
              [ (B.attrName . ('f':) . show . T.rgb $ c, B.fg . T.color $ c)
                    | c <- palette256 ]
              -- background color map: prefix hexcode with 'b'
            , [ (B.attrName . ('b':) . show . T.rgb $ c, B.bg . T.color $ c)
                    | c <- palette256 ]
              -- base attributes
            , [ ("label", Vty.withStyle Vty.defAttr Vty.bold) ]
            ]

---------------------------------------------------------------------
-- Helper functions

breakInto :: Int -> [a] -> [[a]]
breakInto _ [] = []
breakInto n xs = ps : breakInto n ss
    where (ps, ss) = splitAt n xs
