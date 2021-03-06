{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( routeView
    , makeMap
    ) where

import qualified Types                as T
import qualified Graphics.Vty         as Vty
import qualified Brick                as B
import qualified Brick.Widgets.Center as B
import Text.Printf                           ( printf       )
import Data.Ord                              ( comparing    )
import Data.List                             ( intersperse
                                             , minimumBy
                                             , intercalate  )
import Brick                                 ( (<=>), (<+>) )
import Model                                 ( palette256
                                             , palette240
                                             , paletteGreys
                                             , palette16
                                             , breakInto
                                             , matchColor
                                             , sortPalette  )

-- =============================================================== --
-- Interfaces and central router

routeView :: T.Setup -> [ B.Widget T.Name ]
-- ^Central UI router.
routeView st = case T.mode st of
                    T.Block   -> blockUI st
                    T.Cube c  -> cubeUI  c
                    T.Match x -> matchUI x st
                    _         -> ravelUI st

ravelUI :: T.Setup-> [ B.Widget T.Name ]
ravelUI st =
    let ui240 = T.sortDir st . sortPalette (T.sortCode st) $ palette240
        wgts  = map ( ravelLine (T.string st) ) $ palette16 ++ ui240
        ui    = intersperse (hSeparator 3) . map B.vBox . breakInto 64 $ wgts
        note  = "hexcodes may be incorrect for user-defined colors"
        title = B.withAttr "label" . B.str $ note
    in  [ B.viewport T.Swatches B.Both $ title <=> B.hBox ui ]

blockUI :: T.Setup -> [ B.Widget T.Name ]
blockUI st =
    let hdr240 = "\n240-Color Palette (" ++ T.sortCode st ++ "-sorted)"
        hdr16  = "16-Color Palette"
        go     = T.sortDir st . sortPalette (T.sortCode st)
        ui16   = labelRow palette16 <=> swatchRow palette16
        cs240  = breakInto 16 . go $ palette240
        ui240  = B.vBox [ labelRow cs <=> swatchRow cs | cs <- cs240 ]
    in  [ B.viewport T.Swatches B.Both $
              B.vBox [ B.withAttr "label" . B.str $ hdr16
                     , ui16
                     , B.withAttr "label" . B.str $ hdr240
                     , ui240 ] ]

cubeUI :: T.RGBCube -> [ B.Widget T.Name ]
cubeUI (T.RGBCube _ x _) =
    let set16   = breakInto 8 $ palette16
        ui16    = B.vBox [ labelRow cs <=> swatchRow cs | cs <- set16]
        setGrey = breakInto 8 $ paletteGreys
        uiGreys = B.vBox [ labelRow cs <=> swatchRow cs | cs <- setGrey ]
    in  [ cubeWidget x
          <+> hSeparator 3
          <+> B.vBox [ B.withAttr "label" . B.str $ "16-Color Palette"
                     , ui16
                     , B.withAttr "label" . B.str $ "\n24-Color Greyscale"
                     , uiGreys ]
          <=> cubeControls ]

matchUI :: T.RGB -> T.Setup -> [ B.Widget T.Name ]
matchUI c st = [ B.viewport T.Swatches B.Both $ warningWgt <=> query <=> ui ]
    where warningStr = "matches may be incorrect for user-defined colors"
          warningWgt = B.withAttr "label" . B.str $ warningStr
          query      = B.withAttr "label" . B.str $ "query: " ++ show c
          ui         = B.vBox . matchWidget (T.string st)
                              . matchColor (T.matchCount st) $ c

-- =============================================================== --
-- widgets

---------------------------------------------------------------------
-- General helper widgets

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
-- ^Horizontal spacing using default colors.
hSeparator w = B.str . replicate w $ ' '

---------------------------------------------------------------------
-- Help widgets for ravel UI

ravelLine :: String -> T.Color -> B.Widget T.Name
ravelLine s c = B.hBox . intersperse (hSeparator 2) $ ui
    where ui = [ label 7 (show . T.rgb) c
               , label 3 (show . T.code) c
               , coloredString s c
               , swatch 3 c
               ]

matchWidget :: String -> [(T.Color, Int)] -> [B.Widget T.Name]
matchWidget _ [] = []
matchWidget s xs = map (matchLine s m) xs
    where m = snd . minimumBy (comparing snd) $ xs

matchLine :: String -> Int -> (T.Color, Int) -> B.Widget T.Name
matchLine s m (c, d)
    | d == m    = ravelLine s c <+> B.withAttr "label" ( B.str dstr0 )
    | otherwise = ravelLine s c <+> B.withAttr "label" ( B.str dstr  )
    where dstr  = printf " delta RGB = %3d" d
          dstr0 = dstr ++ " -- closest"

---------------------------------------------------------------------
-- Helper widgets for the cube UI

cubeWidget :: T.RGBPlane -> B.Widget T.Name
cubeWidget x = title <=> cubePlane
    where cubePlane = B.vBox [ labelRow cs <=> swatchRow cs | cs <- x ]
          title     = B.withAttr "label" . B.str $ "216-Color Cube"

cubeControls :: B.Widget T.Name
cubeControls = B.withAttr "label" . B.str . intercalate "\n" $ hs
    where hs = [ "Cube Controls:"
               , "Left/Right:    Rotate about Z-axis (into screen)"
               , "Up/Down:       Move along Z-axis (into screen)"
               , "Shift-Up/Down: Rotate about X-axis (left-to-right)"
               , "\nFor more information, try: swatches --help"
               ]

-- =============================================================== --
-- Attributes

colorFG :: T.Color -> B.AttrName
colorFG = B.attrName . ('f':) . show . T.code

colorBG :: T.Color -> B.AttrName
colorBG = B.attrName . ('b':) . show . T.code

getVtyColor :: T.ColorCode -> Maybe Vty.Color
getVtyColor c
    | c < 0     = Nothing
    | c < 16    = Just . Vty.ISOColor . fromIntegral $ c
    | c < 256   = Just . Vty.Color240 . fromIntegral $ c - 16
    | otherwise = Nothing

getDefAttr :: Maybe Vty.Color -> Maybe Vty.Color -> Vty.Attr
getDefAttr Nothing      Nothing      = Vty.defAttr
getDefAttr (Just fgClr) Nothing      = Vty.withForeColor Vty.defAttr fgClr
getDefAttr Nothing      (Just bgClr) = Vty.withBackColor Vty.defAttr bgClr
getDefAttr (Just fgClr) (Just bgClr) = flip Vty.withForeColor fgClr
                                       . flip Vty.withBackColor bgClr
                                       $ Vty.defAttr

makeMap :: T.Setup -> B.AttrMap
makeMap st =
    let bgClr  = T.background st >>= getVtyColor
        fgClr  = T.foreground st >>= getVtyColor
        dattr  = getDefAttr fgClr bgClr
        fgname = B.attrName . ('f':) . show . T.code
        bgname = B.attrName . ('b':) . show . T.code
    in  B.attrMap dattr . concat $
            [ -- foreground color map: prefix hexcode with 'f'
              [ (fgname c, B.fg . T.color $ c) | c <- palette256 ]
              -- background color map: prefix hexcode with 'b'
            , [ (bgname c, B.bg . T.color $ c) | c <- palette256 ]
              -- base attributes
            , [ ("label", dattr ) ]
            ]
