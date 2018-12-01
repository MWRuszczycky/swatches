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
                                              , makeMap           )

main :: IO ()
main = getSetup <$> getArgs >>= either ( putStrLn ) ( runSwatches )

runSwatches :: Setup -> IO ()
runSwatches setup = do
    putEnv $ "TERM=" ++ terminal setup
    void . defaultMain (makeApp setup) $ setup

---------------------------------------------------------------------

makeApp :: Setup -> App Setup e Name
makeApp st = App { appDraw         = routeView
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent  = routeEvent
                 , appStartEvent   = return
                 , appAttrMap      = const . makeMap $ st }
