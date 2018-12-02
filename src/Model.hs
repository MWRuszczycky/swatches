module Model
    ( -- utilities
      breakInto
    , matchColor
    , readHexCode
    , sortPalette
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
    -- RGB cubes
    , initCube666
    , moveZneg
    , moveZpos
    , rotXneg
    , rotXpos
    , rotZneg
    , rotZpos
    ) where

import qualified Types as T
import Types                ( Colorable (..) )
import Numeric              ( readHex        )
import Data.List            ( sortOn
                            , transpose      )

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
-- Helper functions & utilities

breakInto :: Int -> [a] -> [[a]]
breakInto _ [] = []
breakInto n xs = ps : breakInto n ss
    where (ps, ss) = splitAt n xs

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

readHexCode :: String -> Maybe T.RGB
readHexCode []       = Nothing
readHexCode ('#':c)  = readHexCode c
readHexCode ('x':c)  = readHexCode c
readHexCode cs
    | p == "0x"      = readHexCode . drop 2 $ cs
    | length cs == 6 = fmap foo . mapM (go . readHex) . breakInto 2 $ cs
    | otherwise      = Nothing
    where p              = take 2 cs
          go ((n,""):[]) = Just n
          go _           = Nothing
          foo (r:g:b:_)  = T.RGB r g b

colorDistance :: T.RGB -> T.RGB -> Double
colorDistance (T.RGB r g b) (T.RGB r' g' b') = sqrt . fromIntegral $ d2
    where d2 = (r - r')^2 + (g - g')^2 + (b - b')^2

matchColor :: T.RGB -> [(T.Color, Double)]
matchColor c = take 10 . sortOn snd $ [ (x, go x) | x <- palette256 ]
    where go = colorDistance c . T.rgb

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

---------------------------------------------------------------------
-- RGB Cubes
-- Movement is based on a right-handed cartesian coordinate system
-- where the origin is placed at the upper left corner.
-- x-Axis : negative to positive is from left to right.
-- y-Axis : negative to positive is from top to bottom.
-- z-Axis : negative to positive is from front to back of screen.

moveZpos :: T.RGBCube -> T.RGBCube
-- ^Move in the positive Z-direction through the cube. This is
-- equivalent to pulling the cube out of the screen.
moveZpos (T.RGBCube t x [])    = T.RGBCube t     x []
moveZpos (T.RGBCube t x (y:b)) = T.RGBCube (x:t) y b

moveZneg :: T.RGBCube -> T.RGBCube
-- ^Move in the negative Z-direction through the cube. This is
-- equivalent to pushing the cube into the screen.
moveZneg (T.RGBCube []    x b) = T.RGBCube [] x b
moveZneg (T.RGBCube (y:t) x b) = T.RGBCube t  y (x:b)

rotXneg :: T.RGBCube -> T.RGBCube
-- ^Rotate cube clockwise about the pos -> neg x-axis (i.e., z to y).
-- Note that this conserves depth so that teh visual effect will
-- change depending on the depth.
rotXneg (T.RGBCube t x b)
    | null t''  = T.RGBCube [] (head b') (tail b')
    | otherwise = T.RGBCube (tail t'') (head t'') b'
    where d       = length t + 1
          (t',b') = splitAt d . transpose . map reverse $ reverse t ++ (x:b)
          t''     = reverse t'

rotXpos :: T.RGBCube -> T.RGBCube
-- ^Rotate cube clockwise about the neg -> pos x-axis (i.e., y to z).
-- Note that this conserves depth so that the visual effect will
-- change depending on the depth.
rotXpos (T.RGBCube t x b)
    | null t''  = T.RGBCube [] (head b') (tail b')
    | otherwise = T.RGBCube (tail t'') (head t'') b'
    where d       = length t + 1
          (t',b') = splitAt d . map reverse . transpose $ reverse t ++ (x:b)
          t''     = reverse t'

rotZpos :: T.RGBCube -> T.RGBCube
-- ^Rotate cube clockwise about the neg -> pos z-axis (i.e., x to y).
rotZpos (T.RGBCube t x b) = T.RGBCube (map go t) (go x) (map go b)
    where go = map reverse . transpose

rotZneg :: T.RGBCube -> T.RGBCube
-- ^Rotate cube clockwise about the pos -> neg z-axis (i.e., y to x).
rotZneg (T.RGBCube t x b) = T.RGBCube (map go t) (go x) (map go b)
    where go = transpose . map reverse

initCube666 :: T.RGBCube
-- ^Initialize a 6x6x6 rgb cube such that:
-- blue (left/right), green (up/down), red (in/out)
initCube666 = T.RGBCube [] c cs
    where (c:cs) = map (breakInto 6) . breakInto 36 $ palette216
