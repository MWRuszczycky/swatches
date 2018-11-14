module Model
    (
    ) where

import Types  ( RGBIndex     (..)
              , BasicIndex   (..)
              , GreyIndex    (..)
              , ShadeOfColor (..)
              , RGB          (..)
              , Color        (..)
              , Palette      (..)
              , Colorable    (..) )

---------------------------------------------------------------------
-- color values

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
    | otherwise = round $ dm / fromIntegral mx
    where mx = maximum [r, g, b]
          mn = minimum [r, g, b]
          dm = fromIntegral $ mx - mn

value :: RGB -> Int
value (RGB r g b) = maximum [r, g, b]
