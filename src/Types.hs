module Types
    ( Mode  (..)
    , Name  (..)
    , Setup (..)
    ) where

data Name = Swatches deriving ( Ord, Show, Eq )

data Mode = Square | Stacked | Help deriving ( Eq, Show )

data Setup = Setup { mode     :: Mode
                   , terminal :: String
                   }
