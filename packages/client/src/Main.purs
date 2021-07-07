module Main where

import Loglude

import Debug (spy)
import Effect.Class.Console (log)
import Geometry (Context2D, Geometry, Tea, launchTea)
import Geometry.Shapes.Flex (Arrangement(..), withMinimumSize)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Loglude.Cancelable as Cancelable
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Client.VisualGraph.Render (PatternAction(..), renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph

myPattern :: NodeGraph.Pattern
myPattern =
    { name: "Example pattern"
    , arguments: 
        [ NodeGraph.Pin $ NodeGraph.PinId 0
        , NodeGraph.Pin $ NodeGraph.PinId 1
        , NodeGraph.NestedPattern
            { name: "Tuple"
            , arguments: 
                [ NodeGraph.NestedPattern
                    { name: "Zero"
                    , arguments: []
                    }
                , NodeGraph.Pin $ NodeGraph.PinId 2
                ] 
            }
        ] 
    }

myVisualPattern :: VisualGraph.Pattern
myVisualPattern = { position: vec2 100.0 200.0 }

type MyAction = PatternAction

scene :: Context2D -> Tea Unit MyAction
scene context = { context, initialState: unit, render, handleAction, setup }
    where
    setup = const $ pure unit
    handleAction = case _ of
        ClickedPin id -> log $ "Clicked!!! " <> show id 

    arrangements = [ArrangeStart, ArrangeCenter, ArrangeEnd, SpaceBetween, SpaceEvenly]
    
    render :: Ask Context2D => Unit -> Geometry _
    render _ = spy "geometry" $ withMinimumSize $ renderPattern (Just myVisualPattern) myPattern 0.0

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        fixDpi ctx

        let zoom = 2.0
        -- scale ctx { scaleX: zoom, scaleY: zoom }

        Cancelable.perform $ launchTea $ scene ctx
