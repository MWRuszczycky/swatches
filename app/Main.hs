{-# LANGUAGE OverloadedStrings #-}

import System.Posix.Env     ( putEnv )
import System.Environment   ( getArgs )
import Types                ( Name (..) )
import Controller           ( mngEvent)
import Viewer               ( squareUI
                            , sortedUI
                            , theMap )
import Brick.Main           ( App (..)
                            , neverShowCursor
                            , defaultMain )
import Brick.Types          ( Widget )

makeApp :: ( () -> [Widget Name] ) -> App () e Name
makeApp f = App { appDraw         = f
                , appChooseCursor = neverShowCursor
                , appHandleEvent  = mngEvent
                , appStartEvent   = return
                , appAttrMap      = const theMap }

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    args <- getArgs
    let app = case args of
                   ("square":_) -> makeApp squareUI
                   ("sorted":_) -> makeApp sortedUI
                   _            -> makeApp squareUI
    defaultMain app ()
