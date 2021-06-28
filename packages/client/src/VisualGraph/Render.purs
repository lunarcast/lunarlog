module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Geometry.Shapes.Text as Text
import Geometry.TextBaseline as TextBaseline
import Geometry.Types (Geometry)
import Geometry.Types as Geometry
import Geometry.Vector (x, y)
import Graphics.Canvas (Context2D)
import Loglude.Ask (class Ask)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph

renderPattern :: forall a. Ask Context2D => VisualGraph.Pattern -> NodeGraph.Pattern -> Geometry a
renderPattern { position, width } { name } = Geometry.group
    { children: 
        [ Geometry.rect 
            { position
            , size: vec2 width 100
            , fill: "blue"
            }
        , Text.text
            { position: vec2 (x position + 130) (y position)
            , text: name 
            , stroke: "black"
            , fill: "white"
            , baseline: TextBaseline.top 
            }
        ]
    }