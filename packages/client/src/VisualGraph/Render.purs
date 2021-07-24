module Lunarlog.Client.VisualGraph.Render where

import Loglude

import Data.Undefined.NoProblem (undefined)
import Geometry (Axis(..), Geometry, annotate, bounds, x)
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
import Lunarlog.Editor.Types (EditorGeometryId(..), PatternAction(..), Selection, _selectedNode, patternActionWithParent)
import Math (remainder)

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

halfItemSpacing :: Number
halfItemSpacing = itemSpacing / 2.0

pinRadius :: Number
pinRadius = 15.0

---------- Types
data PinSide = LeftPin | RightPin

type RenderPatternInput =
    { lookupPattern :: NodeId -> Maybe NodeGraph.Node
    , pattern :: ReadableRef (Maybe NodeGraph.Pattern)
    , selection :: ReadableRef Selection
    , hoveredPin :: ReadableRef (Maybe PinId)
    , visualPattern :: ReadableRef (Maybe VisualGraph.Pattern)
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
renderPattern :: Ask Context2D => RenderPatternInput -> ReadableRef (Geometry EditorGeometryId PatternAction)
renderPattern { lookupPattern, pattern, visualPattern, nodeId, selection, hoveredPin } = ado
    inner <- ado
        flex <- ado
            pattern <- pattern
            selectionIsNode <- selection <#> (preview _selectedNode >>> maybe false ((/=) nodeId)) # RR.dropDuplicates
            hoveredPin <- hoveredPin
            in case pattern of
                Nothing -> Geometry.None zero
                Just pattern -> Flex.withMinimumSize $ renderPatternLayout 
                    { lookupPattern
                    , pattern
                    , offset: 0.0
                    , nodeId
                    , selectionIsNode
                    , hoveredPin
                    }
        isSelected <- selection <#> (preview _selectedNode >>> maybe false ((==) nodeId)) # RR.dropDuplicates
        in Geometry.group 
            { children: [ flex ]
            , alpha: if not isSelected then 1.0 else 0.7
            }

    position <- visualPattern >>= case _ of
        Nothing -> pure zero 
        Just visualPattern -> RR.readonly visualPattern.position # RR.dropDuplicates

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
            
    argumentGeometries = arguments <#> (identity &&& lookupPattern) <#> uncurry case _, _ of
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
            $ Flex.wrapLayout (withSpacing >>> Geometry.mapAction (patternActionWithParent nodeId))
            $ renderPatternLayout
                { lookupPattern
                , pattern
                , nodeId: childId
                , offset: offset + patternPadding
                , selectionIsNode
                , hoveredPin
                }

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
    weight = 4.0

pin :: forall id. Ask Context2D => PinId -> PinSide -> Number -> Geometry id PatternAction
pin id side extraOffset = pinCircle # Geometry.lockBounds \locked -> Geometry.transform
    { target: locked
    , transform: Transform.translate (vec2 offset 0.0)
    }
    where
    pinCircle = withSpacing $ Geometry.circle
        { radius: pinRadius
        , position: zero
        , fill: pinColor 
        }

    offset = extraOffset + case side of
        LeftPin -> -pinRadius - patternPadding
        RightPin -> pinRadius
