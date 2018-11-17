module Model
    ( -- utilities
      sortPalette
      -- color palettes
    , palette256
    , palette240
    , palette216
    , palette16
    , paletteGreys
    -- RGB to HSV conversion
    , rgbToHSV
    , hue
    , saturation
    , value
    ) where

import qualified Types as T
import Types                ( Colorable (..) )
import Data.List            ( sortOn         )

---------------------------------------------------------------------
-- Palettes

palette256 :: T.Palette
palette256 = palette16 ++ palette240

palette240 :: T.Palette
palette240 = palette216 ++ paletteGreys

palette216 :: T.Palette
palette216 = map toColor cs
    where cs = [ T.RGBColor r g b | r <- vs, g <- vs, b <- vs ]
          vs = [ T.I0 .. T.I5 ]

paletteGreys :: T.Palette
paletteGreys = map toColor cs
    where cs = [ T.GI0 .. T.GI23 ]

palette16 :: T.Palette
palette16 = map toColor cs
    where cs = [ T.Black .. T.White ]

---------------------------------------------------------------------
-- Palette sorting

sortPalette :: T.SortCode -> T.Palette -> T.Palette
sortPalette "hsv"  = sortOn ( rgbToHSV . T.rgb )
sortPalette "hvs"  = sortOn ( (\(h,s,v) -> (h,v,s)) . rgbToHSV . T.rgb )
sortPalette "shv"  = sortOn ( (\(h,s,v) -> (s,h,v)) . rgbToHSV . T.rgb )
sortPalette "svh"  = sortOn ( (\(h,s,v) -> (s,v,h)) . rgbToHSV . T.rgb )
sortPalette "vsh"  = sortOn ( (\(h,s,v) -> (v,s,h)) . rgbToHSV . T.rgb )
sortPalette "vhs"  = sortOn ( (\(h,s,v) -> (v,h,s)) . rgbToHSV . T.rgb )
sortPalette "rgb"  = sortOn ( (\(T.RGB r g b) -> (r,g,b)) . T.rgb )
sortPalette "rbg"  = sortOn ( (\(T.RGB r g b) -> (r,b,g)) . T.rgb )
sortPalette "brg"  = sortOn ( (\(T.RGB r g b) -> (b,r,g)) . T.rgb )
sortPalette "bgr"  = sortOn ( (\(T.RGB r g b) -> (b,g,r)) . T.rgb )
sortPalette "grb"  = sortOn ( (\(T.RGB r g b) -> (g,r,b)) . T.rgb )
sortPalette "gbr"  = sortOn ( (\(T.RGB r g b) -> (g,b,r)) . T.rgb )
sortPalette "ansi" = sortOn T.code
sortPalette _      = sortPalette "svh"

---------------------------------------------------------------------
-- HSV conversion

rgbToHSV :: T.RGB -> (Int, Int, Int)
rgbToHSV x = ( hue x, saturation x, value x)

hue :: T.RGB -> Int
hue (T.RGB r g b)
    | mx == mn  = 0
    | mx == r   = go . (*60) . (+0) . (/) gdb $ dm
    | mx == g   = go . (*60) . (+2) . (/) bdr $ dm
    | otherwise = go . (*60) . (+4) . (/) rdg $ dm
    where mx  = maximum [r, g, b]
          mn  = minimum [r, g, b]
          dm  = fromIntegral $ mx - mn
          gdb = fromIntegral $ g - b
          bdr = fromIntegral $ b - r
          rdg = fromIntegral $ r - g
          go x = round $ if x < 0 then x + 360 else x

saturation :: T.RGB -> Int
saturation (T.RGB r g b)
    | mx == 0   = 0
    | otherwise = round $ 100 * dm / fromIntegral mx
    where mx = maximum [r, g, b]
          mn = minimum [r, g, b]
          dm = fromIntegral $ mx - mn

value :: T.RGB -> Int
value (T.RGB r g b) = round $ 100 * mx / 256
    where mx = fromIntegral . maximum $ [r, g, b]
