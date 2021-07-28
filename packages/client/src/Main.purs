module Main 
    ( main
    , module Lunarlog.VisualGraph.Image
    ) where

import Loglude

import Geometry (launchTea)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Loglude.Cancelable as Cancelable
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Editor (scene)
import Lunarlog.VisualGraph.Image (renderPatternToImage)

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        fixDpi ctx

        let zoom = 2.0

        Cancelable.perform do
            provide ctx $ launchTea scene