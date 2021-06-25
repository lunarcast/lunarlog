module Geometry.Main where

import Loglude

import Control.Monad.State (modify_)
import Data.Vec (vec2)
import Geometry.Types (circle)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Loglude.Cancelable as Cancelable
import Lunarlog.Tea (Tea, launchTea)

data MyAction = IncreaseRadius

scene :: Context2D -> Tea Int MyAction
scene = { context: _, initialState: 10, render, handleAction }
    where
    handleAction = case _ of
        IncreaseRadius -> modify_ (_ + 10)
    
    render state = circle 
        { onClick: const IncreaseRadius
        , fill: "red"
        , position: vec2 (state * 2 + 100) 100
        , radius: state } 

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        Cancelable.perform $ launchTea $ scene ctx

foreign import now :: Effect Number

{-

rect { onSomething: , otherAttribute }

Takes attributes and uses them
Takes events and registers them. Perhaps keep track of some event graph :hinking:

The geometry is a tree. We can put the events in a tree as well

-}