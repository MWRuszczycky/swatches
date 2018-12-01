module Resources
    ( getSetup
    , helpStr
    ) where

import qualified System.Console.GetOpt as Opt
import qualified Types                 as T
import Model                                  ( initCube666 )
import Text.Read                              ( readMaybe   )
import Paths_swatches                         ( version     )
import Data.Version                           ( showVersion )
import Data.List                              ( foldl'
                                              , intercalate )

---------------------------------------------------------------------
-- Help strings

helpStr :: String
helpStr = intercalate "\n" hs
    where hs = [ usage
               , Opt.usageInfo "Options summary (more detail below):" options
               , spacer
               , modeUsage
               , spacer
               , terminalUsage
               , spacer
               , stringUsage
               , spacer
               , sortUsage
               ]

spacer :: String
spacer = replicate 60 '-'

usage :: String
usage = unlines hs
    where hs = [ "Swatches summarizes the colors available in your terminal.\n"
               , "Usage:\n  swatches [MODE] [OPTION..]"
               ]

terminalUsage :: String
terminalUsage = unlines hs
    where hs = [ "-- Set the TERM terminal-ID with the <terminal> option\n"
               , "Set the TERM parameter. The default is xterm-256color."
               ]

modeUsage :: String
modeUsage = unlines hs
    where hs = [ "-- Set the display mode with the [MODE] argument\n"
               , "You can display the colors in the following formats:\n"
               , "  swatches ravel"
               , "    Display a linearly raveled spectrum together with a test"
               , "    string, ansi code and nominal hexcode. Hexcodes may not"
               , "    be correct if colors have been user-defined. Colors are"
               , "    sorted according to the sort code set with the --sort"
               , "    option (see below).\n"
               , "  swatches block"
               , "    Display a 16 x 16 block of swatches labeled with ansi"
               , "    codes. Colors are sorted (see below) along the rows then"
               , "    column according to the sort code set with the --sort"
               , "    option. The first row is always the 16 unsorted 4-bit"
               , "    colors.\n"
               , "  swatches cube"
               , "    Display the 216-color palette using an interactive 6x6x6"
               , "    cube that you can manipulate via the keyboard. The 24"
               , "    greyscale colors and the 16 4-bit colors are also"
               , "    displayed statically. All colors are shown with their"
               , "    respective ansi codes. The cube is manipulated using a"
               , "    right-handed coordinate system with the origin at the"
               , "    upper left corner of the terminal, the y-axis pointing"
               , "    top-to-bottom and the x-axis pointing left-to-right (so"
               , "    the z-axis is pointing into the screen). To manipulate"
               , "    cube, use the following controls:\n"
               , "        UP:         move along z in the positive direction"
               , "        DOWN:       move along z in the negative direction"
               , "        LEFT:       rotate counter clockwise about z"
               , "        RIGHT:      rotate clockwise about z"
               , "        SHIFT-UP:   rotate counter clockwise about x"
               , "        SHIFT-DOWN: rotate clockwise about x\n"
               , "    Rotations about the x-axis are depth-preserving. So, the"
               , "    visual effect will be different depending on whether you"
               , "    are near the top or bottom of the cube."
               ]

stringUsage :: String
stringUsage = unlines hs
    where hs = [ "-- Set the test string to display with the <string> option\n"
               , "Set the string to display in different colors in <ravel>"
                 ++ " mode."
               ]

sortUsage :: String
sortUsage = unlines hs
    where hs = [ "-- Set how the colors should be sorted using the <sort>"
                 ++ " option\n"
               , "Displayed colors can be sorted according to their *intended*"
               , "RGB/HSV values; however, if colors have been redefined, then"
               , "they may not sort correctly. To sort on RGB values, use an"
               , "'rgb' triple in the order you want to sort. For example, to"
               , "sort on the green channel, then blue and finally red use,\n"
               , "     --sort=gbr\n"
               , "To sort on HSV values, do the same with an 'hsv' triple. For"
               , "example, to sort by value, then hue then saturation use,\n"
               , "     --sort=vhs\n"
               , "The default sort is 'svh'. Sorts are *descending* by default."
               , "To sort in ascending order, use the '-a/--ascending' flag."
               , "Finally, you can also sort by ansi code using,\n"
               , "     --sort=ansi"
               ]


versionStr :: String
versionStr = "btx version " ++ showVersion version

---------------------------------------------------------------------
-- State initialization

options :: [ Opt.OptDescr (T.Setup -> T.Setup) ]
options = [ Opt.Option "" ["terminal"]
                ( Opt.ReqArg ( \ arg s -> s { T.terminal = arg } ) "TERM" )
                "Set the TERM terminal-ID (see below)."
          , Opt.Option "b" ["background"]
                ( Opt.ReqArg ( \ arg s -> s { T.background = readMaybe arg } )
                    "CODE" )
                ( "Set the background color according\n"
                  ++ "to the specified ansi code." )
          , Opt.Option "" ["string"]
                ( Opt.ReqArg ( \ arg s -> s { T.string = arg } ) "STRING" )
                "Set the test string."
          , Opt.Option "" ["sort"]
                ( Opt.ReqArg ( \ arg s -> s { T.sortCode = arg } ) "CODE" )
                ( "Set the color sort using a sort code\n"
                  ++ "such as 'rgb', 'shv', etc. (see below)." )
          , Opt.Option "a" ["ascending"]
                ( Opt.NoArg ( \ s -> s { T.sortDir = id } ) )
                "Use an ascending color sort\n(default is descending)."
          , Opt.Option "h" ["help"]
                ( Opt.NoArg ( \ s -> s { T.info = Just helpStr } ) )
                "Display usage information."
          , Opt.Option "v" ["version"]
                ( Opt.NoArg ( \ s -> s { T.info = Just versionStr } ) )
                "Display version."
          ]

setupDef :: T.Setup
setupDef = T.Setup { T.mode       = T.Ravel
                   , T.terminal   = "xterm-256color"
                   , T.background = Nothing
                   , T.string     = "swatches"
                   , T.sortCode   = "svh"
                   , T.sortDir    = reverse
                   , T.info       = Nothing
                   }

setMode :: String -> T.Setup -> T.Setup
setMode "cube"  st = st { T.mode = T.Cube initCube666 }
setMode "ravel" st = st { T.mode = T.Ravel            }
setMode "block" st = st { T.mode = T.Block            }
setMode _       st = st

getSetup :: [String] -> Either String T.Setup
getSetup args =
    case Opt.getOpt Opt.Permute options args of
         ( os, x:_, [] ) -> let st = setMode x . foldl' (flip ($)) setupDef $ os
                            in  maybe (Right st) (Left . id) . T.info $ st
         ( os, [] , [] ) -> let st = foldl' (flip ($)) setupDef os
                            in  maybe (Right st) (Left . id) . T.info $ st
         ( _ , _  , es ) -> Left . unlines $ es
