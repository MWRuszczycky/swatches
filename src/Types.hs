module Types
    ( -- State
      Mode             (..)
    , Name             (..)
    , Setup            (..)
    , SortCode         (..)
    -- Color
    , BasicColor       (..)
    , ChannelIntensity (..)
    , Color            (..)
    , Colorable        (..) -- Class
    , ColorCode        (..)
    , GreyIntensity    (..)
    , Palette          (..)
    , RGB              (..)
    , RGBColor         (..)
    , RGBCube          (..)
    , RGBPlane         (..)
    ) where

import qualified Graphics.Vty as Vty
import Numeric                       ( showHex )

-- =============================================================== --
-- State

data Name = Swatches deriving ( Ord, Show, Eq )

-- |Display mode
data Mode = Cube RGBCube
          | Ravel
          | Block
          | Match RGB
          deriving ( Show )

-- |Programmatic State
data Setup = Setup { mode       :: Mode               -- Display mode
                   , terminal   :: String             -- Terminal settings
                   , background :: Maybe Int          -- Ansi code for backgrnd
                   , string     :: String             -- Display string
                   , sortCode   :: SortCode           -- Color sorting code
                   , sortDir    :: [Color] -> [Color] -- Sorting direction
                   , info       :: Maybe String       -- Info to display to user
                   }                                  -- (e.g., a help string)

-- |Code for sorting colors (e.g, rgb, gbr, hsv, svh, etc.)
type SortCode = String

type RGBPlane = [[Color]]

-- |RGB Cubes are just simple zipper-like structures with a top stack
-- a center plane and a bottom stack of color RGB color planes. The
-- top stack runs head to tail as bottom to top (i.e., in the
-- negative z-direction). The bottom stack runs head to tail as top
-- to bottom (i.e., in the positive z-direction). The center plane is
-- the active plane.
data RGBCube = RGBCube [RGBPlane] RGBPlane [RGBPlane] deriving ( Show )

-- =============================================================== --
-- Color

-- |RGB representation of a colorable instance.
data RGB = RGB { red   :: Int
               , green :: Int
               , blue  :: Int
               } deriving ( Eq )

instance Show RGB where
    show (RGB r g b) = '#' : concatMap go [r, g, b]
        where go x | x < 0     = "00"
                   | x < 16    = '0' : showHex x []
                   | x < 256   = showHex x []
                   | otherwise = "ff"

-- |Ansi color codes.
type ColorCode = Int

-- | Everything needed to describe and display a terminal color.
data Color = Color { code  :: ColorCode     -- Terminal color code
                   , rgb   :: RGB           -- RGB value of color
                   , color :: Vty.Color     -- Vty color value
                   }

instance Show Color where
    show ( Color c r _ ) = "(" ++ show c ++ "," ++ show r ++ ")"

type Palette = [Color]

---------------------------------------------------------------------
-- Colorable class: things that can be converted to colors

class Colorable a where
    toColor :: a -> Color

---------------------------------------------------------------------
-- RGB-type colors

data ChannelIntensity = I0 | I1 | I2 | I3 | I4 | I5
                        deriving ( Eq, Ord, Show, Enum )

data RGBColor = RGBColor ChannelIntensity ChannelIntensity ChannelIntensity
                deriving ( Eq, Show )

instance Colorable RGBColor where
    toColor (RGBColor r g b) =
        let c     = 16 + 36 * fromEnum r + 6 * fromEnum g + fromEnum b
            go I0 = 0
            go I1 = 95
            go x  = 40 + go (pred x)
        in  Color { code  = c
                  , rgb   = RGB (go r) (go g) (go b)
                  , color = Vty.Color240 . fromIntegral $ c - 16
                  }

---------------------------------------------------------------------
-- Greyscale colors

data GreyIntensity = GI0  | GI1  | GI2  | GI3  | GI4  | GI5  | GI6  | GI7  |
                     GI8  | GI9  | GI10 | GI11 | GI12 | GI13 | GI14 | GI15 |
                     GI16 | GI17 | GI18 | GI19 | GI20 | GI21 | GI22 | GI23
                     deriving ( Eq, Ord, Show, Enum )

instance Colorable GreyIntensity where
    toColor x = Color c (RGB v v v) z
        where c = 232 + fromEnum x
              v = 10 * fromEnum x + 8
              z = Vty.Color240 . fromIntegral $ c - 16

---------------------------------------------------------------------
-- Basic 4-bit colors

data BasicColor = Black | Maroon  | Green | Olive  |
                  Navy  | Purple  | Teal  | Silver |
                  Grey  | Red     | Lime  | Yellow |
                  Blue  | Fuchsia | Aqua | White
                  deriving ( Eq, Show, Enum )

instance Colorable BasicColor where
    toColor x = Color (encode x) (toRGB x) (go x)
        where go            = Vty.ISOColor . fromIntegral . encode
              toRGB Black   = RGB   0   0   0
              toRGB Maroon  = RGB 128   0   0
              toRGB Green   = RGB   0 128   0
              toRGB Olive   = RGB 128 128   0
              toRGB Navy    = RGB   0   0 128
              toRGB Purple  = RGB 128   0 128
              toRGB Teal    = RGB   0 128 128
              toRGB Silver  = RGB 192 192 192
              toRGB Grey    = RGB 128 128 128
              toRGB Red     = RGB 255   0 255
              toRGB Lime    = RGB   0 255   0
              toRGB Yellow  = RGB 255 255   0
              toRGB Blue    = RGB   0   0 255
              toRGB Fuchsia = RGB 255   0 255
              toRGB Aqua    = RGB   0 255 255
              toRGB White   = RGB 255 255 255
              encode Black   =  0
              encode Maroon  =  1
              encode Green   =  2
              encode Olive   =  3
              encode Navy    =  4
              encode Purple  =  5
              encode Teal    =  6
              encode Silver  =  7
              encode Grey    =  8
              encode Red     =  9
              encode Lime    = 10
              encode Yellow  = 11
              encode Blue    = 12
              encode Fuchsia = 13
              encode Aqua    = 14
              encode White   = 15
