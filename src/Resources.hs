module Resources
    ( getSetup
    , helpStr
    ) where

import qualified System.Console.GetOpt as Opt
import qualified Types                 as T
import Data.List                              ( foldl' )

---------------------------------------------------------------------
-- Help strings

helpStr :: String
helpStr = "Help string (to be written)."

---------------------------------------------------------------------
-- State initialization

options :: [ Opt.OptDescr (T.Setup -> T.Setup) ]
options = [ Opt.Option "t" ["terminal"]
                ( Opt.ReqArg ( \ arg s -> s { T.terminal = arg } ) "TERMINAL" )
                "Set the terminal (defualt is xterm-256-color)"
          , Opt.Option "m" ["mode"]
                ( Opt.ReqArg ( \ arg s -> s { T.mode = setMode arg } ) "MODE" )
                "Set the display mode."
          , Opt.Option "t" ["test"]
                ( Opt.ReqArg ( \ arg s -> s { T.testString = arg } ) "STRING" )
                "Set the test string."
          , Opt.Option "s" ["sort"]
                ( Opt.ReqArg ( \ arg s -> s { T.sortCode = arg } ) "STRING" )
                "Set the color sort."
          , Opt.Option "h" ["help"]
                ( Opt.NoArg ( \ s -> s { T.mode = T.Help } ) )
                "Display usage information."
          ]

setupDef :: T.Setup
setupDef = T.Setup { T.mode       = T.Spectrum
                   , T.terminal   = "xterm-256color"
                   , T.testString = "swatches"
                   , T.sortCode   = "svh"
                   }

setMode :: String -> T.Mode
setMode "cube"     = T.Cube
setMode "spectrum" = T.Spectrum
setMode "block"    = T.Block
setMode _          = T.Spectrum

getSetup :: [String] -> Either String T.Setup
getSetup args = case Opt.getOpt Opt.Permute options args of
                     ( os, _ , [] ) -> Right $ foldl' ( flip ($) ) setupDef os
                     ( _ , _ , es ) -> Left . unlines $ es
