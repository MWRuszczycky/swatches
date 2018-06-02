{-# LANGUAGE OverloadedStrings #-}

import System.Posix.Env     ( putEnv )
import Types                ( Name (..) )
import Controller           ( mngEvent)
import Viewer               ( drawUI
                            , theMap )
import Brick.Main           ( App (..)
                            , neverShowCursor
                            , defaultMain )

app :: App () e Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = mngEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap }

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    defaultMain app ()
