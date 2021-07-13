module Main where

import Loglude

import Geometry (launchTea)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Loglude.Cancelable as Cancelable
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Editor (myVisualPattern, scene)

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        fixDpi ctx

        let zoom = 2.0

        Cancelable.perform do
            pattern <- liftEffect myVisualPattern
            provide ctx $ launchTea $ scene pattern