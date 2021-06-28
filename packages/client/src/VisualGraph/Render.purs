module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Geometry.Shapes.Padding as Padding
import Geometry.Shapes.Text as Text
import Geometry.TextBaseline as TextBaseline
import Geometry.Types (Geometry)
import Geometry.Types as Geometry
import Graphics.Canvas (Context2D)
import Loglude.Ask (class Ask)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph

renderPattern :: forall a. Ask Context2D => VisualGraph.Pattern -> NodeGraph.Pattern -> Geometry a
renderPattern { position, width } { name } = Geometry.group
    { children: 
        [ Geometry.rect 
            { position
            , size: vec2 width 100.0
            , fill: "blue"
            }
        , Padding.aabbPadding
            { target: Text.text
                { position
                , text: name 
                , stroke: "black"
                , baseline: TextBaseline.top 
                }
            , amount: Padding.equalPadding 30.0
            , alpha: 0.3
            , fill: "yellow"
            }
        ]
    }