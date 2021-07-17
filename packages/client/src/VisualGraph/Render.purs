module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Debug (spy)
import Geometry (Axis(..), Geometry)
import Geometry as Geometry
import Geometry.Shapes.Flex (FlexLayout)
import Geometry.Shapes.Flex as Flex
import Geometry.TextBaseline as TextBaseline
import Geometry.Transform as Transform
import Graphics.Canvas (Context2D)
import Loglude.ReactiveRef as RR
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorGeometryId(..), PatternAction(..), patternActionWithParent)

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
data PinSide = LeftPin | RightPin

type RenderPatternInput =
    { lookupPattern :: NodeId -> Maybe NodeGraph.Node
    , pattern :: NodeGraph.Pattern
    , visualPattern :: VisualGraph.Pattern
    , nodeId :: NodeId
    }

type RenderNestedPatternInput =
    { lookupPattern :: NodeId -> Maybe NodeGraph.Node
    , pattern :: NodeGraph.Pattern
    , offset :: Number
    , nodeId :: NodeId
    }

---------- Implementation
renderPattern :: Ask Context2D => RenderPatternInput -> ReadableRef (Geometry EditorGeometryId PatternAction)
renderPattern { lookupPattern, pattern, visualPattern, nodeId } = ado
    let 
      inner = spy "inner" $ Flex.withMinimumSize $ renderPatternLayout 
        { lookupPattern
        , pattern
        , offset: 0.0
        , nodeId
        }
    position <- RR.readonly visualPattern.position # RR.dropDuplicates
    in Geometry.transform
        { transform: Transform.translate position
        , target: inner
        }

renderPatternLayout :: Ask Context2D => RenderNestedPatternInput -> FlexLayout EditorGeometryId PatternAction
renderPatternLayout { lookupPattern, pattern: { name, arguments }, offset, nodeId } = Flex.createFlexLayout
    { flexAxis: Y
    , stretchChildren: true
    , wrap: \child -> Geometry.reporter
        { id: NodeGeometry nodeId
        , reportAbsoluteBounds: true
        , target: Geometry.aabbPadding
            { target: child
            , amount: Geometry.fourWayPadding patternPadding itemSpacing 0.0 itemSpacing
            , paddingModifiers:
                { fill: patternBackground 
                , stroke: patternStrokeColor
                , weight: 3.0
                }
            , parentModifiers:
                { onClick: \e -> SelectNode e $ pure nodeId
                }
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
    argumentGeometries = arguments <#> (identity &&& lookupPattern) <#> uncurry case _, _ of
        _, Nothing -> Flex.NotLayout $ Geometry.None zero
        nodeId, Just (NodeGraph.Unify id) -> Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , arrangeChildren: Flex.SpaceBetween
            , children: [ Flex.NotLayout $ pin id LeftPin $ -offset, Flex.NotLayout $ pin id RightPin 0.0 ]
            }  
        childId, Just (NodeGraph.PatternNode pattern) -> Flex.IsLayout
            $ Flex.wrapLayout (withSpacing >>> Geometry.mapAction (patternActionWithParent nodeId))
            $ renderPatternLayout
                { lookupPattern
                , pattern
                , nodeId: childId
                , offset: offset + patternPadding
                }

withSpacing :: forall id a. Ask Context2D => Geometry id a -> Geometry id a
withSpacing target = Geometry.aabbPadding
    { target
    , amount: Geometry.fourWayPadding 0.0 itemSpacing 0.0 0.0
    }

pin :: forall id. Ask Context2D => PinId -> PinSide -> Number -> Geometry id PatternAction
pin id side extraOffset = Geometry.transform
    { target: withSpacing $ Geometry.circle
        { radius: pinRadius
        , position: zero
        , fill: pinColor 
        -- , onClick: const $ ClickedPin id
        }
    , transform: Transform.translate (vec2 offset 0.0)
    , transformBounds: false
    }
    where
    offset = extraOffset + case side of
        LeftPin -> -pinRadius - patternPadding
        RightPin -> pinRadius
