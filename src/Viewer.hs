{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( drawUI
    , theMap
    ) where

import Data.List            ( intersperse )
import Brick.Util           ( on )
import Types                ( Name (..) )
import Brick.Types          ( Widget
                            , ViewportType (..) )
import Brick.Widgets.Center ( center
                            , hCenter)
import Brick.Widgets.Core   ( str
                            , withAttr
                            , hBox
                            , vBox
                            , hLimit
                            , fill
                            , vLimit
                            , viewport
                            , cropToContext )
import Brick.AttrMap        ( AttrMap
                            , attrMap
                            , attrName )
import Graphics.Vty         ( Color (..)
                            , withStyle
                            , bold
                            , white
                            , black
                            , defAttr )

---------------------------------------------------------------------
-- UI renderer

drawUI :: [Widget Name]
drawUI = [ uiScroll ]
    where ui       = vBox . shuffleIn codes $ swatches
          values   = [ [ 16 * r + c | c <- [0..15] ] | r <- [0..15] ]
          swatches = map colorValues values
          codes    = map ( numberValues 5 ) values
          uiScroll = viewport Swatches Both $ ui

---------------------------------------------------------------------
-- M.Named widgets

blkSep :: Widget Name
blkSep = withAttr "csep" . str $ " "

clrBlock :: Widget Name
clrBlock = str . replicate 5 $ ' '

---------------------------------------------------------------------
-- Widget contructors

colorValues :: [Int] -> Widget Name
colorValues r = hBox . intersperse blkSep $ clrs
    where clrs = map ( \ x -> withAttr ( attrName . show $ x ) clrBlock ) r

numberValues :: Int -> [Int] -> Widget Name
numberValues w r = hBox . intersperse blkSep $ nums
    where fmt  = hLimit w . hCenter . str . show
          nums = map ( withAttr "vsep" . fmt ) r

---------------------------------------------------------------------
-- Attribute map

theMap :: AttrMap
theMap = attrMap defAttr $ clrsISO ++ clrs240 ++ clrsDisp
    where clrs240  = [ ( attrName . show $ n
                       , white `on` Color240 (n - 16) ) | n <- [16..255] ]
          clrsISO  = [ ( attrName . show $ n
                       , white `on` ISOColor n ) | n <- [0..15] ]
          clrsDisp = [ ( "csep", black `on` black )
                     , ( "vsep", withStyle ( white `on` black ) bold ) ]

---------------------------------------------------------------------
-- Helper functions

shuffleIn :: [a] -> [a] -> [a]
shuffleIn [] ys         = ys
shuffleIn xs []         = xs
shuffleIn (x:xs) (y:ys) = x:y:( shuffleIn xs ys )
