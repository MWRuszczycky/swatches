{-# LANGUAGE OverloadedStrings #-}

import System.Posix.Env     ( putEnv )
import System.Environment   ( getArgs )
import Types                ( Name (..) )
import Controller           ( mngEvent)
import Viewer               ( squareUI
                            , sortedUI
                            , theMap
                            , usage )
import Brick.Main           ( App (..)
                            , neverShowCursor
                            , defaultMain )
import Brick.Types          ( Widget )

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    args <- getArgs
    let cmd = if null args then "square" else head args
    case lookup cmd cmdHub of
         Just c  -> c
         Nothing -> putStrLn . concat $ [ "unrecognized command\n"
                                        , "try: swatches help" ]

cmdHub :: [ (String, IO ()) ]
cmdHub = [ ( "square", defaultMain ( makeApp squareUI ) () )
         , ( "sorted", defaultMain ( makeApp sortedUI ) () )
         , ( "help"  , usage ) ]

makeApp :: ( () -> [Widget Name] ) -> App () e Name
makeApp f = App { appDraw         = f
                , appChooseCursor = neverShowCursor
                , appHandleEvent  = mngEvent
                , appStartEvent   = return
                , appAttrMap      = const theMap }
