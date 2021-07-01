module Main where

import Loglude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Typelevel.Undefined (undefined)
import Debug (spy)
import Geometry (Context2D, Geometry, Tea, bounds, launchTea)
import Geometry as Geometry
import Geometry.Shapes.Flex (FlexAxis(..), createLayout, fixedSizeLayout)
import Geometry.Shapes.Padding as Padding
import Graphics.Canvas (getCanvasElementById, getContext2D, scale)
import Loglude.Cancelable as Cancelable
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph

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
    render _ = Geometry.group 
        { children: 
           [ Geometry.rect 
                { position: vec2 100.0 200.0
                , size: layout.minimumSize
                , fill: "blue" 
                }
           , geom
           ] 
        }
        where
        bruh = spy "bounds" $ bounds geom
        geom = spy "geometry" $ fixedSizeLayout layout layout.minimumSize
        layout = spy "layout" $ createLayout
            { children: bgify <$> NonEmptyArray.cons' a [b, c] 
            , position: vec2 100.0 200.0
            , flexAxis: FlexY
            , stretchChildren: false
            , arrangeChildren: undefined
            , alignChildren: undefined
            }

        bgify g = Geometry.aabbPadding 
                    { target: g
                    , amount: Geometry.equalPadding 10.0 
                    , fill: "yellow"
                    , paddingPlacement: Padding.FixedCorner
                    }

        a = Geometry.text { text: "Adriel", font: "30px Arial", position: zero, stroke: "black" } 
        b = Geometry.text { text: "Short", position: zero, fill: "black" } 
        c = Geometry.text { text: "Long name!", font: "20px Source Code Pro", position: zero, fill: "black" } 
        -- spy "geometry" $ 
        -- renderPattern myVisualPattern myPattern

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        fixDpi ctx

        let zoom = 2.0
        scale ctx { scaleX: zoom, scaleY: zoom }

        Cancelable.perform $ launchTea $ scene ctx
