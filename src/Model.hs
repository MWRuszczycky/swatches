module Model
    ( palette256
    , palette240
    , paletteGreys
    , palette16
    , sortPalette
    , rgbToHSV
    ) where

import Data.List ( sortOn            )
import Types     ( RGBIndex     (..)
                 , BasicIndex   (..)
                 , GreyIndex    (..)
                 , ShadeOfColor (..)
                 , RGB          (..)
                 , Color        (..)
                 , Palette      (..)
                 , Colorable    (..) )

---------------------------------------------------------------------
-- Palettes

palette256 :: Palette
palette256 = palette16 ++ palette240 ++ paletteGreys

palette240 :: Palette
palette240 = map toColor cs
    where cs = [ RGBIndex r g b | r <- vs, g <- vs, b <- vs ]
          vs = [ CS0 .. CS5 ]

paletteGreys :: Palette
paletteGreys = map toColor cs
    where cs = [ GS0 .. GS23 ]

palette16 :: Palette
palette16 = map toColor cs
    where cs = [ Black .. White ]

---------------------------------------------------------------------
-- Palette sorting

sortPalette :: String -> Palette -> Palette
sortPalette "hsv"  = reverse . sortOn ( rgbToHSV . rgb )
sortPalette "hvs"  = reverse . sortOn ( (\(h,s,v) -> (h,v,s)) . rgbToHSV . rgb )
sortPalette "shv"  = reverse . sortOn ( (\(h,s,v) -> (s,h,v)) . rgbToHSV . rgb )
sortPalette "svh"  = reverse . sortOn ( (\(h,s,v) -> (s,v,h)) . rgbToHSV . rgb )
sortPalette "vsh"  = reverse . sortOn ( (\(h,s,v) -> (v,s,h)) . rgbToHSV . rgb )
sortPalette "vhs"  = reverse . sortOn ( (\(h,s,v) -> (v,h,s)) . rgbToHSV . rgb )
sortPalette "rgb"  = reverse . sortOn ( (\(RGB r g b) -> (r,g,b)) . rgb )
sortPalette "rbg"  = reverse . sortOn ( (\(RGB r g b) -> (r,b,g)) . rgb )
sortPalette "brg"  = reverse . sortOn ( (\(RGB r g b) -> (b,r,g)) . rgb )
sortPalette "bgr"  = reverse . sortOn ( (\(RGB r g b) -> (b,g,r)) . rgb )
sortPalette "grb"  = reverse . sortOn ( (\(RGB r g b) -> (g,r,b)) . rgb )
sortPalette "gbr"  = reverse . sortOn ( (\(RGB r g b) -> (g,b,r)) . rgb )
sortPalette "ansi" = sortOn code
sortPalette _      = sortPalette "svh"

---------------------------------------------------------------------
-- HSV conversion

rgbToHSV :: RGB -> (Int, Int, Int)
rgbToHSV x = ( hue x, saturation x, value x)

hue :: RGB -> Int
hue (RGB r g b)
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

saturation :: RGB -> Int
saturation (RGB r g b)
    | mx == 0   = 0
    | otherwise = round $ 100 * dm / fromIntegral mx
    where mx = maximum [r, g, b]
          mn = minimum [r, g, b]
          dm = fromIntegral $ mx - mn

value :: RGB -> Int
value (RGB r g b) = round $ 100 * mx / 256
    where mx = fromIntegral . maximum $ [r, g, b]
