module Main where

import Loglude

import Data.Array as Array
import Effect.Class.Console (log)
import Geometry (Context2D, Geometry, Tea, launchTea, x, y)
import Geometry as Geometry
import Geometry.Shapes.Flex (Alignment(..), Arrangement(..), LayoutChild(..), createFlexLayout, withFixedSize, withMinimumSize)
import Geometry.Shapes.Padding as Padding
import Geometry.Vector (Axis(..))
import Graphics.Canvas (getCanvasElementById, getContext2D)
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
myVisualPattern = { position: vec2 100.0 200.0 }

data MyAction = Clicked String

scene :: Context2D -> Tea Unit MyAction
scene context = { context, initialState: unit, render, handleAction, setup }
    where
    setup = const $ pure unit
    handleAction = case _ of
        Clicked msg -> log $ "Clicked!!! " <> msg 

    arrangements = [ArrangeStart, ArrangeCenter, ArrangeEnd, SpaceBetween, SpaceEvenly]
    
    render :: Ask Context2D => Unit -> Geometry _
    render _ = withMinimumSize $ createFlexLayout
            { children: IsLayout <$> Array.mapWithIndex renderArrangement arrangements
            , position: zero
            , flexAxis: X
            , stretchChildren: true
            }

    renderArrangement :: Ask Context2D => Int -> Arrangement -> _
    renderArrangement index arrangement = createFlexLayout
        { children:
           [ NotLayout $ Geometry.aabbPadding  
                { target: Geometry.text 
                    { text: show arrangement
                    , font: "20px Source Code Pro"
                    , position: zero
                    , fill: "black"
                    }
                , amount: Geometry.equalPadding 10.0
                }
            , NotLayout $ Geometry.aabbPadding
                { target: Geometry.group 
                    { children:  
                        [ Geometry.rect 
                            { position: vec2 0.0 0.0
                            , size 
                            , fill: "blue" 
                            }
                        , geom
                        ]
                    }
                , amount: Geometry.equalPadding 8.0
                }
           ] 
        , position: zero
        , flexAxis: Y
        , stretchChildren: true
        , alignChildren: AlignMiddle
        }
        where
        geom = withFixedSize layout size 
        size = vec2 (x layout.minimumSize) (y layout.minimumSize + 100.0)
        layout = createFlexLayout
            { children: NotLayout <$> addPadding <$> [a, b, c] 
            , position: vec2 0.0 0.0
            , flexAxis: Y
            , arrangeChildren: arrangement
            , alignChildren: AlignMiddle
            }

        addPadding g = Geometry.aabbPadding 
                    { target: g
                    , amount: Geometry.equalPadding 10.0 
                    , fill: "yellow"
                    , onClick: const $ Clicked "something else"
                    , paddingPlacement: Padding.FixedCorner
                    }

        a = Geometry.text { text: "Something", onClick: const $ Clicked "something", font: "30px Arial", position: zero, stroke: "black" } 
        b = Geometry.text { text: "Short", position: zero, fill: "black" } 
        c = Geometry.text { text: "Long text!", font: "20px Source Code Pro", position: zero, fill: "black" } 

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas ->  do
        ctx <- getContext2D canvas
        fixDpi ctx

        let zoom = 2.0
        -- scale ctx { scaleX: zoom, scaleY: zoom }

        Cancelable.perform $ launchTea $ scene ctx
