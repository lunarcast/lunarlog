module Geometry.Main where

import Loglude

import Control.Monad.State (modify_)
import Data.Int (floor)
import Data.Vec (vec2)
import FRP.Event.AnimationFrame (animationFrame)
import Geometry.Performance as Performance
import Geometry.Types (ClickCheck(..), circle)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Loglude.Cancelable as Cancelable
import Lunarlog.Tea (Tea, launchTea)
import Math (sin)

data MyAction = IncreaseRadius | MoveAnimation Number
type MyState = { radius :: Int, x :: Int }

scene :: Context2D -> Tea MyState MyAction
scene = { context: _, initialState: { radius: 20, x: 100 }, render, handleAction, setup }
    where
    setup { propagateAction } = do
        Cancelable.subscribe animationFrame $ const do
            time <- Performance.now 
            propagateAction $ MoveAnimation time 

    handleAction = case _ of
        IncreaseRadius -> modify_ \s -> s { radius = s.radius + 10 }
        MoveAnimation time -> modify_ _ { x = floor $ 40.0 * sin (time / 100.0) }
    
    render { radius, x } = circle 
        { onClick: const IncreaseRadius
        , clickChecker: MouseCloserThan 100.0
        , fill: "red"
        , position: vec2 (100 + x) 100
        , radius } 

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        Cancelable.perform $ launchTea $ scene ctx
