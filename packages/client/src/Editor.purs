module Lunarlog.Editor where

import Loglude

import Control.Apply (lift2, lift3)
import Data.Aged as Aged
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Compactable (compact)
import Data.HashMap as HashMap
import Data.Lens.Index (ix)
import Data.MouseButton (nothingPressed)
import Data.Traversable (sequence)
import Effect.Aff (launchAff_)
import FRP.Stream as Stream
import Geoemtry.Data.AABB as AABB
import Geometry (Geometry, MultiStepRenderer, Tea, Vec2, ReporterOutput, _position, x)
import Geometry as Geometry
import Geometry.Base (mapAction)
import Geometry.Tea (TeaM, createMouseEvent, eventStream, stopPropagation)
import Graphics.Canvas (Context2D)
import Loglude.Cancelable as Cancelable
import Loglude.Data.BiHashMap as BiHashMap
import Loglude.Data.Lens (_atHashMap)
import Loglude.Editor.Actions (dropPattern, rememberMousePosition, selectNestedNode, selectNode, selectPin, updateHovered)
import Loglude.Editor.Components.Connection (connection)
import Loglude.Run.ExternalState (assign, get, modifying, use)
import Lunarlog.Client.VisualGraph.Render (renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction(..), EditorGeometryId(..), EditorState, PatternAction(..), PinSide(..), Selection(..), _hovered, _mousePosition, _nestedPinDropZone, _ruleConnections, _ruleNode, _selectedNode, _selectedPin, _selection, _visualRule, _visualRuleNode)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

---------- Constants
rule :: NodeGraph.Rule
rule = NodeGraph.Rule
    { head: NodeId 0
    , body: [NodeId 0, NodeId 6]
    , nodes: HashMap.fromArray $ mkNodes "Exmaple pattern" 0 <> mkNodes "Super duper mega long name" 6
    , connections: BiHashMap.empty
    }
    where
    mkNodes name id = 
        [ NodeId id /\ NodeGraph.PatternNode
            { name
            , arguments: NodeId <$> [id + 1, id + 2, id + 3]
            }
        , NodeId (id + 1) /\ NodeGraph.Unify (NodeGraph.PinId id)
        , NodeId (id + 2) /\ NodeGraph.Unify (NodeGraph.PinId $ id + 1)
        , NodeId (id + 3) /\ NodeGraph.PatternNode
            { name: "Tuple"
            , arguments:  NodeId <$> [id + 4, id + 5]
            }
        , NodeId (id + 4) /\ NodeGraph.PatternNode
            { name: "Zero"
            , arguments: []
            }
        , NodeId (id + 5) /\ NodeGraph.Unify (NodeGraph.PinId $ id + 2)
        ] 

myVisualPattern :: VisualGraph.Rule
myVisualPattern = do
    let node1 = { position: vec2 100.0 200.0 }
    let node2 = { position: vec2 400.0 200.0 }
    { connections: HashMap.empty
    , nodes: HashMap.fromArray
        [ NodeId 0 /\ VisualGraph.PatternNode node1
        , NodeId 6 /\ VisualGraph.PatternNode node2
        ]
    }

---------- Implementation
scene :: Ask Context2D => Tea EditorState EditorGeometryId EditorAction
scene =
    { initialState: 
        { visualRule: myVisualPattern
        , rule: rule
        , selection: NoSelection
        , hovered: []
        , mouseMove: empty
        , mousePosition: zero
        , nextId: intToNat 13
        }
    , render
    , handleAction
    , setup
    }
    where
    setup { propagateAction } = do
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mousemove) $ (createMouseEvent >>> MouseMove >>> propagateAction >>> launchAff_)
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mouseup) $ (createMouseEvent >>> MouseUp >>> propagateAction >>> launchAff_)

    handleAction :: EditorAction -> TeaM EditorState EditorGeometryId EditorAction Unit
    handleAction = case _ of
        NodeAction (SelectNode event path) -> do
            rememberMousePosition event
            let nodeId = NonEmptyArray.head path
            selectNode nodeId
            case NonEmptyArray.index path 1 of
                Nothing -> pure unit
                Just parent -> selectNestedNode { parent, nodeId }
            -- We do not want to select more than one thing at once
            stopPropagation
        NodeAction (SelectPin event pinId path) -> do
            rememberMousePosition event

            for_ (Array.last path) (selectPin pinId)

            -- We do not want to select more than one thing at once
            stopPropagation
        RefreshSelection event -> do
            when (nothingPressed event.buttons) do
                use _selection >>= case _ of
                    SelectedNode id -> do
                        dropPattern id
                        removeSelection
                    _ -> pure unit
            where
            removeSelection = assign _selection NoSelection
        MouseUp event -> do
            handleAction $ RefreshSelection event
        MouseMove event -> do
            oldPosition <- get <#> view _mousePosition
            rememberMousePosition event

            handleAction $ RefreshSelection event
            updateHovered

            get <#> preview (_selection <<< _selectedNode) >>= traverse_ \id -> do
                    let delta = event.worldPosition - oldPosition
                    modifying (_visualRule <<< VisualGraph._ruleNodes <<< _atHashMap id <<< _Just <<< VisualGraph._patternNode <<< _position) ((+) delta)

    render :: Stream.Discrete EditorState -> Stream.Discrete (MultiStepRenderer _ _)
    render state = ado
        step1 <- step1 state
        step2 <- step2 state
        in step1 /\ [step2]
        
    step1 :: Stream.Discrete EditorState -> Stream.Discrete (Geometry _ _)
    step1 state = state 
            <#> _.rule
            # Aged.dropDuplicates
            <#> (view NodeGraph._ruleBody &&& view NodeGraph._ruleNodes)
            # flip Stream.bind \(bodyNodes /\ nodes) -> Stream.do
                let 
                  renderNode :: NodeId -> _
                  renderNode nodeId = Stream.do
                    geometry <- renderPattern 
                        { lookupPattern: flip HashMap.lookup nodes
                        , nodeId
                        , visualPattern: state <#> preview (_visualRuleNode nodeId <<< VisualGraph._patternNode) 
                            # compact # Aged.dropDuplicates
                        , pattern: state <#> preview (_ruleNode nodeId <<< NodeGraph._patternNode)
                            # compact # Aged.dropDuplicates
                        , selection: state <#> _.selection # Aged.dropDuplicates
                        , hoveredPin: state <#> preview (_hovered <<< ix 0 <<< _nestedPinDropZone)
                            # Aged.dropDuplicates
                            # Aged.dropDuplicatesOn _Just
                        }
                    pure $ mapAction NodeAction geometry
                bodyNodes
                    # map renderNode 
                    # sequence
                    # map (\children -> Geometry.group { children })

    step2 :: Stream.Discrete EditorState -> Stream.Discrete (ReporterOutput _ -> Geometry _ _)
    step2 state = ado
        connections <- connections state
        preview <- connectionPreview state 
        in \report -> Geometry.group
            { children: [connections report, preview report]
            }

lookupBothPinSides :: 
    ReporterOutput EditorGeometryId -> 
    NodeGraph.PinId -> 
    Maybe (Vec2 /\ Vec2)
lookupBothPinSides report pinId = lift2 (/\) (lookupPin LeftPin) (lookupPin RightPin)
    where
    lookupPin side = do
        boundingBox <- HashMap.lookup (PinGeometry pinId side) report.absoluteBounds
        pure $ AABB.center boundingBox

connections :: 
    Ask Context2D => 
    Stream.Discrete EditorState -> 
    Stream.Discrete (ReporterOutput EditorGeometryId -> Geometry EditorGeometryId EditorAction)
connections state =state
    <#> view _ruleConnections
    # Aged.dropDuplicates
    <#> BiHashMap.connections
    <#> \connections report -> Geometry.group
        { children: connections <#> 
            \(from /\ to) -> renderConnection 
                (lookupBothPinSides report from) 
                (lookupBothPinSides report to)
        }

connectionPreview :: 
    Ask Context2D => 
    Stream.Discrete EditorState -> 
    Stream.Discrete (ReporterOutput EditorGeometryId -> Geometry EditorGeometryId EditorAction)
connectionPreview state = ado
    selectedPin <- state 
        <#> preview (_selection <<< _selectedPin) 
        # Aged.dropDuplicatesOn _Just
    mousePosition <- state <#> view _mousePosition # Aged.dropDuplicates
    in \report -> renderConnection 
        (selectedPin >>= lookupBothPinSides report)
        (Just (mousePosition /\ mousePosition))


renderConnection :: Maybe (Vec2 /\ Vec2) -> Maybe (Vec2 /\ Vec2) -> Geometry EditorGeometryId EditorAction
renderConnection left right = 
        case start, lift3 pickSide right middleRight (map x start) of
            Just from, Just to -> connection { from, to } 
            _, _ -> Geometry.None zero
        where
        start = lift3 pickSide left middleLeft middleRight
        
        middleLeft = middle <$> left
        middleRight = middle <$> right

        middle (left /\ right) = (x left + x right) / 2.0

        pickSide (left /\ right) thisMiddle otherMiddle = if otherMiddle > thisMiddle then right else left

    