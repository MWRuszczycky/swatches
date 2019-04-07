module Resources
    ( getSetup
    , helpStr
    ) where

import qualified System.Console.GetOpt as Opt
import qualified Types                 as T
import Model                                  ( initCube666
                                              , readHexCode )
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
    where hs = [ "-- Set the TERM terminal-ID with the --terminal option\n"
               , "Set the TERM parameter. The default is xterm-256color."
               ]

modeUsage :: String
modeUsage = unlines hs
    where hs = [ "-- Set the display mode with the [MODE] argument\n"
               , "You can display the colors in the following formats:\n"
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
               , "    are near the top or bottom of the cube.\n"
               , "  swatches match HEXCODE"
               , "    Given a color hexcode HEXCODE, display samples of the ten"
               , "    closest values in the available 256-color RGB space based"
               , "    on default RGB values. The HEXCODE format must be "
               , "    compatible with shell reformatting, and letters can be"
               , "    either uppercase or lowercase. For example, all of the"
               , "    following will work to match color hexcode #cc715d:\n"
               , "        swatches match cc715d"
               , "        swatches match CC715D"
               , "        swatches match \\#cc715d"
               , "        swatches match '#cc715d'"
               , "        swatches match 0xCC715D"
               , "        ...\n"
               , "    Some matches may be incorrect if color values have been"
               , "    redefined from their default values, because only default"
               , "    RGB values are used to compute distances in RGB space.\n"
               , "  swatches ravel"
               , "    Display a linearly raveled spectrum together with a test"
               , "    string, ansi code and nominal hexcode. Hexcodes may not"
               , "    be correct if colors have been user-defined. Colors are"
               , "    sorted according to the sort code set with the --sort"
               , "    option (see below). The first 16 colors are always the"
               , "    the unsorted 4-bit colors, since these are typically"
               , "    user-defined.\n"
               ]

stringUsage :: String
stringUsage = unlines hs
    where hs = [ "-- Set the test string to display with the --string option\n"
               , "Set the string to display in different colors in ravel-mode."
               ]

sortUsage :: String
sortUsage = unlines hs
    where hs = [ "-- Set how the colors should be sorted using the --sort"
                 ++ " option\n"
               , "Displayed colors can be sorted according to their *default*"
               , "RGB/HSV values; however, if colors have been redefined, then"
               , "they may not sort correctly. Colors are sorted using a sort"
               , "code consisting of:\n"
               , "     r : red channel"
               , "     g : green channel"
               , "     b : blue channel"
               , "     h : hue"
               , "     s : saturation"
               , "     v : value\n"
               , "For example, to sort on the green channel, then blue and"
               , "finally red, use the option:\n"
               , "     --sort=gbr\n"
               , "Likewise, to sort by hue and then the red channel, use:\n"
               , "     --sort=hr\n"
               , "Unrecognized characters in the sort code are just ignored."
               , "The default sort code is 'svh'. Sorts are *descending* by"
               , "default. For ascending sorts, use the '-a/--ascending' flag."
               , "Finally, you can also sort by ansi code using,\n"
               , "     --sort=ansi"
               ]


versionStr :: String
versionStr = "swatches version " ++ showVersion version

---------------------------------------------------------------------
-- State initialization

options :: [ Opt.OptDescr (T.Setup -> T.Setup) ]
options =
    [ Opt.Option "t" ["terminal"]
          ( Opt.ReqArg ( \ arg s -> s { T.terminal = arg } ) "TERM"
          )
          "Set the TERM terminal-ID (see below)."
    , Opt.Option "b" ["background"]
          ( Opt.ReqArg ( \ arg s -> s { T.background = readMaybe arg } ) "CODE"
          )
          "Set the background color according\nto the specified ansi code."
    , Opt.Option "f" ["foreground"]
          ( Opt.ReqArg ( \ arg s -> s { T.foreground = readMaybe arg } ) "CODE"
          )
          "Set the foreground color according\nto the specified ansi code."
    , Opt.Option "x" ["string"]
          ( Opt.ReqArg ( \ arg s -> s { T.string = arg } ) "STRING"
          )
          "Set the test string."
    , Opt.Option "s" ["sort"]
          ( Opt.ReqArg ( \ arg s -> s { T.sortCode = arg } ) "CODE"
          )
          ( "Set the color sort using a sort code\n"
            ++ "such as 'rgb', 'shv', etc. (see below)."
          )
    , Opt.Option "a" ["ascending"]
          ( Opt.NoArg ( \ s -> s { T.sortDir = id } )
          )
          "Use an ascending color sort\n(default is descending)."
    , Opt.Option "m" ["matches"]
          ( Opt.ReqArg ( \ arg s -> let go Nothing  = 10
                                        go (Just x) | x > 0     = x
                                                    | otherwise = 10
                                    in  s { T.matchCount = go . readMaybe $ arg
                                          }
                       )
            "NUMBER"
          )
          "Number of matches to show (default is 10)."
    , Opt.Option "h" ["help"]
          ( Opt.NoArg ( \ s -> s { T.info = Just helpStr } )
          )
          "Display usage information."
    , Opt.Option "v" ["version"]
          ( Opt.NoArg ( \ s -> s { T.info = Just versionStr } )
          )
          "Display version."
    ]

setupDef :: T.Setup
setupDef = T.Setup { T.mode       = T.Ravel
                   , T.terminal   = "xterm-256color"
                   , T.background = Nothing
                   , T.foreground = Nothing
                   , T.string     = "swatches"
                   , T.sortCode   = "svh"
                   , T.sortDir    = reverse
                   , T.info       = Nothing
                   , T.matchCount = 10
                   }

setMatchMode :: T.Setup -> Maybe T.RGB -> Either String T.Setup
setMatchMode st (Just x) = return st { T.mode = T.Match x }
setMatchMode _  Nothing  = Left errMsg
    where errMsg = "Bad hexcode to match\nTry: swatches --help"

setMode :: [String] -> T.Setup -> Either String T.Setup
setMode ("cube" :_ ) st = return st { T.mode = T.Cube initCube666 }
setMode ("ravel":_ ) st = return st { T.mode = T.Ravel            }
setMode ("block":_ ) st = return st { T.mode = T.Block            }
setMode ("match":xs) st = setMatchMode st . readHexCode . concat $ xs
setMode _            st = return st

getSetup :: [String] -> Either String T.Setup
getSetup args =
    case Opt.getOpt Opt.Permute options args of
         ( os, [], [] ) -> let st = foldl' (flip ($)) setupDef os
                           in  maybe (return st) (Left . id) . T.info $ st
         ( os, xs, [] ) -> do st <- setMode xs . foldl' (flip ($)) setupDef $ os
                              maybe (return st) (Left . id) . T.info $ st
         ( _ , _ , es ) -> Left . unlines $ es
