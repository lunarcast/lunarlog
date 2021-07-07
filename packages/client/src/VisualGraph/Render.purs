module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Geometry (Axis(..), Geometry)
import Geometry as Geometry
import Geometry.Shapes.Flex (FlexLayout)
import Geometry.Shapes.Flex as Flex
import Geometry.TextBaseline as TextBaseline
import Geometry.Transform as Transform
import Graphics.Canvas (Context2D)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (PatternArgument(..), PinId(..))
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Constants
patternBackground :: String
patternBackground = "#272345"

patternStrokeColor :: String
patternStrokeColor = "#1A1730"

patternTextColor :: String
patternTextColor = "#DEEE93"

pinColor :: String
pinColor = "#1E88E5"

patternFont :: String
patternFont = "22px Source Code Pro"

patternPadding :: Number
patternPadding = 30.0

pinRadius :: Number
pinRadius = 15.0

---------- Types
data PatternAction = ClickedPin PinId

data PinSide = LeftPin | RightPin

---------- Implementation
renderPattern :: Ask Context2D => Maybe (VisualGraph.Pattern) -> NodeGraph.Pattern -> Number -> FlexLayout PatternAction
renderPattern maybePosition { name, arguments } offset = Flex.createFlexLayout
    { position: maybe zero _.position maybePosition
    , flexAxis: Y
    , stretchChildren: true
    , wrap: \child -> Geometry.aabbPadding
            { target: child
            , weight: 3.0
            , amount: Geometry.fourWayPadding patternPadding 10.0 0.0 $ maybe 0.0 (const 20.0) maybePosition
            , fill: patternBackground 
            , stroke: patternStrokeColor
            , onClick: const $ ClickedPin $ PinId 0
            }
    , children: 
        [ Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , enforceSize: true
            , children: pure $ Flex.NotLayout $ Geometry.aabbPadding
                { target: Geometry.text
                    { position: zero
                    , text: name 
                    , fill: patternTextColor
                    , font: patternFont
                    , baseline: TextBaseline.top
                    }
                , amount: Geometry.fourWayPadding 10.0 0.0 20.0 10.0
                }
            }
        ] <> argumentGeometries
    }
    where
    argumentGeometries = arguments <#> case _ of
        Pin id -> Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , arrangeChildren: Flex.SpaceBetween
            , children: [ Flex.NotLayout $ pin id LeftPin $ -offset, Flex.NotLayout $ pin id RightPin 0.0 ]
            }  
        NestedPattern pattern -> Flex.IsLayout $ renderPattern Nothing pattern $ offset + patternPadding

pin :: PinId -> PinSide -> Number -> Geometry PatternAction
pin id side extraOffset = Geometry.transformed_ $ Geometry.Transformed 
    { target: Geometry.aabbPadding 
        { target: Geometry.circle
            { radius: pinRadius
            , position: zero
            , fill: pinColor 
            , onClick: const $ ClickedPin id
            }
        , amount: Geometry.fourWayPadding 0.0 10.0 0.0 10.0
        }
    , transform: Transform.translate (vec2 offset 0.0)
    , transformBounds: false
    }
    where
    offset = extraOffset + case side of
        LeftPin -> -pinRadius - patternPadding
        RightPin -> pinRadius
