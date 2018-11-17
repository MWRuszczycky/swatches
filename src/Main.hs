{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                          ( void              )
import System.Posix.Env                       ( putEnv            )
import System.Environment                     ( getArgs           )
import Brick                                  ( App (..)
                                              , neverShowCursor
                                              , defaultMain
                                              , Widget            )
import Types                                  ( Name  (..)
                                              , Setup (..)        )
import Resources                              ( getSetup          )
import Controller                             ( routeEvent        )
import Viewer                                 ( routeView
                                              , theMap            )

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
