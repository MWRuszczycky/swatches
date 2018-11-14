module Types
    ( Mode         (..)
    , Name         (..)
    , Color        (..)
    , RGB          (..)
    , Palette      (..)
    , RGBIndex     (..)
    , GreyIndex    (..)
    , ShadeOfColor (..)
    , ShadeOfGrey  (..)
    , Ansi         (..)
    , Setup        (..)
    , Colorable    (..)
    ) where

import qualified Graphics.Vty as T
import Numeric ( showHex )

-- =============================================================== --
-- State

data Name = Swatches deriving ( Ord, Show, Eq )

data Mode = Square | Stacked | Help deriving ( Eq, Show )

data Setup = Setup { mode     :: Mode
                   , terminal :: String
                   }

-- =============================================================== --
-- Color

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

-- |Terminal color codes.
type ColorCode = Int

-- | Everything needed to describe and display a terminal color.
data Color = Color { code  :: ColorCode     -- Terminal color code
                   , rgb   :: RGB           -- RGB value of color
                   , color :: T.Color       -- Vty color value for
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

data ShadeOfColor = CS0 | CS1 | CS2 | CS3 | CS4 | CS5
                    deriving ( Eq, Ord, Show, Enum )

data RGBIndex = RGBIndex ShadeOfColor ShadeOfColor ShadeOfColor
                deriving ( Eq, Show )

instance Colorable RGBIndex where
    toColor (RGBIndex r g b) =
        let c      = 16 + 36 * fromEnum r + 6 * fromEnum g + fromEnum b
            go CS0 = 0
            go CS1 = 95
            go x   = 40 + go (pred x)
        in  Color { code  = c
                  , rgb   = RGB (go r) (go g) (go b)
                  , color = T.Color240 . fromIntegral $ c - 16
                  }

---------------------------------------------------------------------
-- Greyscale colors

data ShadeOfGrey = GS0  | GS1  | GS2  | GS3  | GS4  | GS5  | GS6  | GS7  |
                   GS8  | GS9  | GS10 | GS11 | GS12 | GS13 | GS14 | GS15 |
                   GS16 | GS17 | GS18 | GS19 | GS20 | GS21 | GS22 | GS23
                   deriving ( Eq, Ord, Show, Enum )

data GreyIndex = GreyIndex ShadeOfGrey deriving ( Eq, Show )

instance Colorable GreyIndex where
    toColor (GreyIndex x) = Color c (RGB v v v) z
        where c = 232 + fromEnum x
              v = 10 * fromEnum x + 8
              z = T.Color240 . fromIntegral $ c - 16

---------------------------------------------------------------------
-- Ansi colors and their bold version

data Ansi= Black | Maroon | Green | Olive  | Navy | Purple  | Teal | Silver |
           Grey  | Red    | Lime  | Yellow | Blue | Fuchsia | Aqua | White
           deriving ( Eq, Show, Enum )

instance Colorable Ansi where
    toColor x = Color (encode x) (toRGB x) (go x)
        where go            = T.ISOColor . fromIntegral . encode
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
