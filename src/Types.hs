module Types
    ( Mode  (..)
    , Name  (..)
    , RGB   (..)
    , Setup (..)
    ) where

import Numeric ( showHex )

-- =============================================================== --

data Name = Swatches deriving ( Ord, Show, Eq )

data Mode = Square | Stacked | Help deriving ( Eq, Show )

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

data Setup = Setup { mode     :: Mode
                   , terminal :: String
                   }
