module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Geometry (Axis(..), Geometry, CanvasMouseEvent)
import Geometry as Geometry
import Geometry.Shapes.Effectful as Effectful
import Geometry.Shapes.Flex (FlexLayout)
import Geometry.Shapes.Flex as Flex
import Geometry.TextBaseline as TextBaseline
import Geometry.Transform as Transform
import Graphics.Canvas (Context2D)
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
renderPattern :: Ask Context2D => VisualGraph.Pattern -> NodeGraph.Pattern -> Geometry PatternAction
renderPattern { position: ref } pattern = 
    Effectful.withReactiveRef ref \position -> 
        Geometry.transformed
            { transform: Transform.translate position
            , onClick: const $ SelectNode $ NodeId 0
            , target: inner }
    where
    inner = Flex.withMinimumSize $ renderPatternLayout pattern 0.0

renderPatternLayout :: Ask Context2D => NodeGraph.Pattern -> Number -> FlexLayout PatternAction
renderPatternLayout { name, arguments } offset = Flex.createFlexLayout
    { flexAxis: Y
    , stretchChildren: true
    , wrap: \child -> Geometry.aabbPadding
            { target: child
            , weight: 3.0
            , amount: Geometry.fourWayPadding patternPadding itemSpacing 0.0 itemSpacing
            , fill: patternBackground 
            , stroke: patternStrokeColor
            , onClick: const $ ClickedPin $ PinId 1000
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
                , amount: Geometry.fourWayPadding 0.0 0.0 patternPadding 0.0
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
        NestedPattern pattern -> Flex.IsLayout $ Flex.wrapLayout withSpacing $ renderPatternLayout pattern $ offset + patternPadding

withSpacing :: forall a. Geometry a -> Geometry a
withSpacing target = Geometry.aabbPadding
    { target
    , amount: Geometry.fourWayPadding 0.0 itemSpacing 0.0 0.0
    }

pin :: PinId -> PinSide -> Number -> Geometry PatternAction
pin id side extraOffset = Geometry.transformed 
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
