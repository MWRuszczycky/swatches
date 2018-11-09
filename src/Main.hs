{-# LANGUAGE OverloadedStrings #-}

import qualified System.Console.GetOpt as Opt
import System.Posix.Env                       ( putEnv            )
import System.Environment                     ( getArgs           )
import Data.List                              ( foldl'            )
import Brick                                  ( App (..)
                                              , neverShowCursor
                                              , defaultMain
                                              , Widget            )
import Types                                  ( Name  (..)
                                              , Setup (..)
                                              , Mode  (..)        )
import Controller                             ( mngEvent          )
import Viewer                                 ( squareUI
                                              , sortedUI
                                              , theMap
                                              , usage             )

main :: IO ()
main = getSetup <$> getArgs >>= either ( putStrLn ) ( runSwatches )

runSwatches :: Setup -> IO ()
runSwatches setup = do
    putEnv $ "TERM=" ++ terminal setup
    cmdHub . mode $ setup

cmdHub :: Mode -> IO ()
cmdHub Square  = defaultMain ( makeApp squareUI ) ()
cmdHub Stacked = defaultMain ( makeApp sortedUI ) ()
cmdHub Help    = usage

---------------------------------------------------------------------

makeApp :: ( () -> [Widget Name] ) -> App () e Name
makeApp f = App { appDraw         = f
                , appChooseCursor = neverShowCursor
                , appHandleEvent  = mngEvent
                , appStartEvent   = return
                , appAttrMap      = const theMap }

---------------------------------------------------------------------

setupDef :: Setup
setupDef = Setup { mode     = Square
                 , terminal = "xterm-256color"
                 }

options :: [ Opt.OptDescr (Setup -> Setup) ]
options = [ Opt.Option "t" ["terminal"]
                ( Opt.ReqArg ( \ arg s -> s { terminal = arg } ) "TERMINAL" )
                "Set the terminal (defualt is xterm-256-color)"
          , Opt.Option "s" ["stacked"]
                ( Opt.NoArg ( \ s -> s { mode = Stacked } ) )
                "Display colors stacked along the green coordinate."
          , Opt.Option "h" ["help"]
                ( Opt.NoArg ( \ s -> s { mode = Help } ) )
                "Display usage information."
          ]

getSetup :: [String] -> Either String Setup
getSetup args = case Opt.getOpt Opt.Permute options args of
                     ( os, _ , [] ) -> Right $ foldl' ( flip ($) ) setupDef os
                     ( _ , _ , es ) -> Left . unlines $ es
