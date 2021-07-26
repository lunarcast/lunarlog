module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Data.Aged as Aged
import Data.Undefined.NoProblem (undefined)
import FRP.Stream as Stream
import Geometry (Axis(..), Geometry, annotate, bounds, x)
import Geometry as Geometry
import Geometry.Shapes.Flex (FlexLayout, mapLayoutChild)
import Geometry.Shapes.Flex as Flex
import Geometry.TextBaseline as TextBaseline
import Geometry.Transform as Transform
import Graphics.Canvas (Context2D)
import Loglude.Editor.Settings (connectionPreviewWeight, halfItemSpacing, itemSpacing, patternBackground, patternFont, patternPadding, patternStrokeColor, patternTextColor, pinColor, pinRadius)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorGeometryId(..), PatternAction(..), PinSide(..), Selection, _selectedNode, patternActionWithParent)

---------- Types
type RenderPatternInput =
    { lookupPattern :: NodeId -> Maybe NodeGraph.Node
    , pattern :: Stream.Discrete NodeGraph.Pattern
    , selection :: Stream.Discrete Selection
    , hoveredPin :: Stream.Discrete (Maybe PinId)
    , visualPattern :: Stream.Discrete VisualGraph.Pattern
    , nodeId :: NodeId
    }

type RenderNestedPatternInput =
    { lookupPattern :: NodeId -> Maybe NodeGraph.Node
    , selectionIsNode :: Boolean
    , hoveredPin :: Maybe PinId
    , pattern :: NodeGraph.Pattern
    , offset :: Number
    , nodeId :: NodeId
    }

---------- Implementation
renderPattern :: Ask Context2D => RenderPatternInput -> Stream.Discrete (Geometry EditorGeometryId PatternAction)
renderPattern { lookupPattern, pattern, visualPattern, nodeId, selection, hoveredPin } = ado
    inner <- ado
        flex <- ado
            pattern <- pattern
            selectionIsNode <- selection <#> (preview _selectedNode >>> maybe false ((/=) nodeId)) # Aged.dropDuplicates
            hoveredPin <- hoveredPin
            in Flex.withMinimumSize $ renderPatternLayout 
                { lookupPattern
                , pattern
                , offset: 0.0
                , nodeId
                , selectionIsNode
                , hoveredPin
                }
        isSelected <- selection <#> (preview _selectedNode >>> maybe false ((==) nodeId)) # Aged.dropDuplicates
        in Geometry.group 
            { children: [ flex ]
            , alpha: if not isSelected then 1.0 else 0.7
            }

    position <- visualPattern <#> _.position

    in Geometry.transform
        { transform: Transform.translate position
        , target: inner
        }

renderPatternLayout :: Ask Context2D => RenderNestedPatternInput -> FlexLayout EditorGeometryId PatternAction
renderPatternLayout { lookupPattern, pattern: { name, arguments }, offset, nodeId, selectionIsNode, hoveredPin } = Flex.createFlexLayout
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
                { onMousedown: \e -> SelectNode e $ pure nodeId
                }
            }
        }
    , children: 
        [ Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , enforceSize: true
            , children: [label]
            }
        ] <> argumentGeometries
    }
    where
    label = Flex.NotLayout $ Geometry.aabbPadding
        { target: Geometry.text
            { position: zero
            , text: name 
            , fill: patternTextColor
            , font: patternFont
            , baseline: TextBaseline.top
            }
        , amount: Geometry.fourWayPadding 0.0 0.0 patternPadding 0.0
        }

    applyOffset target = Geometry.transform 
        { transform: Transform.translate $ vec2 offset 0.0
        , target
        }
    
    dashedLines id target
        | not selectionIsNode = []
        | otherwise = maybe [] pure ado
            targetBounds <- bounds target
            let xOffset = -(offset + patternPadding)
            in Geometry.group
                { children:
                    [ dashedPinConnector
                        { y: halfItemSpacing
                        , patternWidth: x targetBounds.size
                        , xOffset
                        , sign: 1.0
                        }
                    , dashedPinConnector
                        { y: halfItemSpacing + 2.0 * pinRadius
                        , patternWidth: x targetBounds.size
                        , xOffset
                        , sign: -1.0
                        }
                    , Geometry.reporter
                        { id: NestedPinDropZone id
                        , reportAbsoluteBounds: true
                        , target: Geometry.rect
                            { position: vec2 xOffset 0.0
                            , size: vec2 (x targetBounds.size - xOffset) (2.0 * pinRadius + itemSpacing)
                            , fill: if hoveredPin == Just id 
                                then opt pinColor 
                                else undefined
                            , alpha: 0.7
                            }
                        }
                    ]
                }
            
    argumentGeometries = arguments 
        <#> (identity &&& lookupPattern) 
        <#> uncurry case _, _ of
        _, Nothing -> Flex.NotLayout $ Geometry.None zero
        nodeId, Just (NodeGraph.Unify id) -> Flex.IsLayout $ Flex.createFlexLayout
            { flexAxis: X
            , arrangeChildren: Flex.SpaceBetween
            , wrap:  Geometry.lockBounds \target -> annotate (NodeGeometry nodeId) $ Geometry.group
                { children: [ target ] <> dashedLines id target
                }
            , children: [ Flex.NotLayout $ pin id LeftPin $ -offset, Flex.NotLayout $ pin id RightPin 0.0 ]
            }  
        childId, Just (NodeGraph.PatternNode pattern) -> Flex.IsLayout
            $ Flex.wrapLayout withSpacing
            $ renderPatternLayout
                { lookupPattern
                , pattern
                , nodeId: childId
                , offset: offset + patternPadding
                , selectionIsNode
                , hoveredPin
                }
        <#> mapLayoutChild (Geometry.mapAction (patternActionWithParent nodeId))

withSpacing :: forall id a. Ask Context2D => Geometry id a -> Geometry id a
withSpacing target = Geometry.aabbPadding
    { target
    , amount: Geometry.xyPadding $ vec2 0.0 (itemSpacing / 2.0)
    }

dashedPinConnector :: forall id a. { patternWidth :: Number, xOffset :: Number, y :: Number, sign :: Number } -> Geometry id a
dashedPinConnector { patternWidth, y, xOffset, sign } = Geometry.line
    { from: vec2 xOffset adjustedY
    , to: vec2 patternWidth adjustedY
    , stroke: pinColor
    , weight
    , dash: [10.0, 10.0]
    , dashOffset: (totalWidth - 10.0) `remainder` 20.0
    , alpha: 0.7
    }
    where
    totalWidth = patternWidth - xOffset
    adjustedY = y + weight * sign / 2.0
    weight = connectionPreviewWeight

pin :: Ask Context2D => PinId -> PinSide -> Number -> Geometry EditorGeometryId PatternAction
pin id side extraOffset = pinCircle # Geometry.lockBounds \locked -> annotate (PinGeometry id side) $ Geometry.transform
    { target: locked
    , transform: Transform.translate (vec2 offset 0.0)
    }
    where
    pinCircle = withSpacing $ Geometry.circle
        { radius: pinRadius
        , position: zero
        , fill: pinColor 
        , onMousedown: \event -> SelectPin event id []
        }

    offset = extraOffset + case side of
        LeftPin -> -pinRadius - patternPadding
        RightPin -> pinRadius
