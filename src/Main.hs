{-# LANGUAGE OverloadedStrings #-}

import qualified System.Console.GetOpt as Opt
import Control.Monad                          ( void              )
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
import Controller                             ( routeEvent        )
import Viewer                                 ( routeView
                                              , theMap
                                              , usage             )

main :: IO ()
main = getSetup <$> getArgs >>= either ( putStrLn ) ( runSwatches )

runSwatches :: Setup -> IO ()
runSwatches setup = do
    putEnv $ "TERM=" ++ terminal setup
    print . mode $ setup
    void . defaultMain theApp $ setup

---------------------------------------------------------------------

theApp :: App Setup e Name
theApp = App { appDraw         = routeView
             , appChooseCursor = neverShowCursor
             , appHandleEvent  = routeEvent
             , appStartEvent   = return
             , appAttrMap      = const theMap }

---------------------------------------------------------------------

setupDef :: Setup
setupDef = Setup { mode       = Spectrum "svh"
                 , terminal   = "xterm-256color"
                 , testString = "abcdefghijklmnopqrstuvwxyz0123456789"
                 }

options :: [ Opt.OptDescr (Setup -> Setup) ]
options = [ Opt.Option "t" ["terminal"]
                ( Opt.ReqArg ( \ arg s -> s { terminal = arg } ) "TERMINAL" )
                "Set the terminal (defualt is xterm-256-color)"
          , Opt.Option "m" ["mode"]
                ( Opt.ReqArg ( \ arg s -> s { mode = Spectrum arg } ) "MODE" )
                "Set the display mode."
          , Opt.Option "h" ["help"]
                ( Opt.NoArg ( \ s -> s { mode = Help } ) )
                "Display usage information."
          ]

getSetup :: [String] -> Either String Setup
getSetup args = case Opt.getOpt Opt.Permute options args of
                     ( os, _ , [] ) -> Right $ foldl' ( flip ($) ) setupDef os
                     ( _ , _ , es ) -> Left . unlines $ es
