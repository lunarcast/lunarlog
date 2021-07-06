module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Graphics.Canvas (Context2D)
import Geometry (Geometry)
import Geometry as Geometry
import Geometry.TextBaseline as TextBaseline
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph as NodeGraph

renderPattern :: forall a. Ask Context2D => VisualGraph.Pattern -> NodeGraph.Pattern -> Geometry a
renderPattern { position } { name } = Geometry.group
    { children: 
        [ Geometry.aabbPadding
            { target: Geometry.text
                { position
                , text: name 
                , stroke: "black"
                , baseline: TextBaseline.top 
                }
            , amount: Geometry.equalPadding 30.0
            , alpha: 0.3
            , fill: "yellow"
            }
        ]
    }