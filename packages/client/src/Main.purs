module Main where

import Prelude

import Debug (spy)
import Geometry.Types (Geometry)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Loglude (Effect, traverse_, vec2)
import Loglude.Ask (class Ask)
import Loglude.Cancelable as Cancelable
import Lunarlog.Client.VisualGraph.Render (renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Tea (Tea, launchTea)

myPattern :: NodeGraph.Pattern
myPattern =
    { name: "Example pattern"
    , arguments: 
        [ NodeGraph.Pin $ NodeGraph.PinId 0
        , NodeGraph.NestedPattern
            { name: "Zero"
            , arguments: [] 
            }
        ] 
    }

myVisualPattern :: VisualGraph.Pattern
myVisualPattern = { position: vec2 100.0 200.0, width: 100.0 }

scene :: Context2D -> Tea Unit Unit
scene context = { context, initialState: unit, render, handleAction, setup }
    where
    setup = const $ pure unit
    handleAction = const $ pure unit
    
    render :: Ask Context2D => Unit -> Geometry Unit
    render _ = spy "geometry" $ renderPattern myVisualPattern myPattern

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        Cancelable.perform $ launchTea $ scene ctx
