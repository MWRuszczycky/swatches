module Model
    (
    ) where

import Types  ( RGBIndex     (..)
              , GreyIndex    (..)
              , ShadeOfColor (..)
              , ShadeOfGrey  (..)
              , Ansi         (..)
              , Colorable    (..) )
---------------------------------------------------------------------
-- color values

rgbIndices :: [RGBIndex]
rgbIndices = [ RGBIndex r g b | r <- vs, g <- vs, b <- vs ]
    where vs = [ CS0 .. CS5 ]

greyIndices :: [ GreyIndex ]
greyIndices = map GreyIndex [ GS0 .. GS23 ]

ansiIndices :: [ Ansi ]
ansiIndices = [ Black .. White ]
