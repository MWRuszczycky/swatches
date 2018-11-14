module Model
    (
    ) where

import Types  ( RGBIndex     (..)
              , RGB          (..)
              , GreyIndex    (..)
              , ShadeOfColor (..)
              , ShadeOfGrey  (..)
              , Ansi         (..)
              , Colorable    (..) )
---------------------------------------------------------------------
-- color values

rgbIndices :: [RGBIndex]
rgbIndices = [ RGBIndex r g b | r <- vs, g <- vs, b <- vs ]
    where vs = [ CS0 .. CS5 ]

greyIndices :: [ GreyIndex ]
greyIndices = map GreyIndex [ GS0 .. GS23 ]

ansiIndices :: [ Ansi ]
ansiIndices = [ Black .. White ]

-- rgbToHSV :: RGB -> HSV
-- rgbToHSV (RGB r g b) =

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
