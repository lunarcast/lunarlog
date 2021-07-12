module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Debug (spy)
import Geometry (Axis(..), CanvasMouseEvent, Geometry)
import Geometry as Geometry
import Geometry.Shapes.Flex (FlexLayout)
import Geometry.Shapes.Flex as Flex
import Geometry.TextBaseline as TextBaseline
import Geometry.Transform as Transform
import Graphics.Canvas (Context2D)
import Loglude.ReactiveRef as RR
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId(..), PatternArgument(..), PinId(..))
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

itemSpacing :: Number
itemSpacing = 16.0

pinRadius :: Number
pinRadius = 15.0

---------- Types
data PatternAction 
    = ClickedPin PinId
    | SelectNode NodeId
    | MouseMove CanvasMouseEvent
    | MouseUp CanvasMouseEvent
    | RefreshSelection CanvasMouseEvent

data PinSide = LeftPin | RightPin

---------- Implementation
renderPattern :: Ask Context2D => VisualGraph.Pattern -> ReadableRef (NodeGraph.Pattern) -> ReadableRef (Geometry PatternAction)
renderPattern { position } patterns = do
    inner <- patterns <#> \pattern -> spy "inner" $ Flex.withMinimumSize $ renderPatternLayout pattern 0.0

    RR.readonly position # RR.dropDuplicates <#> \position -> 
        Geometry.transform
            { transform: Transform.translate position
            , onClick: const $ SelectNode $ NodeId 0
            , target: inner
            }

renderPatternLayout :: Ask Context2D => NodeGraph.Pattern -> Number -> FlexLayout PatternAction
renderPatternLayout { name, arguments } offset = Flex.createFlexLayout
    { flexAxis: Y
    , stretchChildren: true
    , wrap: \child -> Geometry.aabbPadding
            { target: child
            , amount: Geometry.fourWayPadding patternPadding itemSpacing 0.0 itemSpacing
            , paddingModifiers:
                { fill: patternBackground 
                , stroke: patternStrokeColor
                , weight: 3.0
                , onClick: const $ ClickedPin $ PinId 1000
                }
            }
    , children: 
        [ Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , enforceSize: true
            , children: [ Flex.NotLayout $ Geometry.aabbPadding
                { target: Geometry.text
                    { position: zero
                    , text: name 
                    , fill: patternTextColor
                    , font: patternFont
                    , baseline: TextBaseline.top
                    }
                , amount: Geometry.fourWayPadding 0.0 0.0 patternPadding 0.0
                }]
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
        NestedPattern pattern -> Flex.IsLayout $ Flex.wrapLayout withSpacing $ renderPatternLayout pattern $ offset + patternPadding

withSpacing :: forall a. Ask Context2D => Geometry a -> Geometry a
withSpacing target = Geometry.aabbPadding
    { target
    , amount: Geometry.fourWayPadding 0.0 itemSpacing 0.0 0.0
    }

pin :: Ask Context2D => PinId -> PinSide -> Number -> Geometry PatternAction
pin id side extraOffset = Geometry.transform
    { target: withSpacing $ Geometry.circle
        { radius: pinRadius
        , position: zero
        , fill: pinColor 
        , onClick: const $ ClickedPin id
        }
    , transform: Transform.translate (vec2 offset 0.0)
    , transformBounds: false
    }
    where
    offset = extraOffset + case side of
        LeftPin -> -pinRadius - patternPadding
        RightPin -> pinRadius
