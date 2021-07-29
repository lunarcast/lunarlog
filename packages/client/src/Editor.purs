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
import Data.Undefined.NoProblem as Opt
import Effect.Aff (launchAff_)
import FRP.Stream (previewMap)
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
import Loglude.Editor.Actions (createBranch, createNode, deleteBranch, deleteConnection, deleteNode, dropPattern, editBranch, rememberMousePosition, selectNestedNode, selectNode, selectPin, updateHovered, usesPointerEvents)
import Loglude.Editor.Components.Connection (connection)
import Loglude.Editor.Settings (hoveredConnectionWeight)
import Loglude.Run.ExternalState (assign, get, modifying, use)
import Lunarlog.Client.VisualGraph.Render (renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction(..), EditorGeometryId(..), EditorState, ForeignAction(..), InitialState, KeyboardAction(..), PatternAction(..), PinSide(..), Selection(..), _hovered, _hoveredConnection, _mousePosition, _nestedPinDropZone, _pointerEventsEnabled, _rule, _ruleConnections, _ruleHead, _ruleNode, _selectedNode, _selectedPin, _selection, _visualRule, _visualRuleNode)
import Prelude (const, when, zero)
import Web.Event.Event (EventType(..))
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

---------- Implementation
scene :: Ask Context2D => InitialState -> Tea EditorState EditorGeometryId EditorAction
scene initial =
    { initialState: 
        { visualRule: { nodes: HashMap.empty }
        , module: HashMap.empty
        , selection: NoSelection
        , hovered: []
        , mousePosition: zero
        , nextId: zero
        , currentRule: Nothing
        , pointerEventsEnabled: false
        }
    , render
    , handleAction
    , setup
    }
    where
    setup { propagateAction } = do
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mousemove) $ (createMouseEvent >>> MouseMove >>> propagateAction >>> launchAff_)
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mouseup) $ (createMouseEvent >>> MouseUp >>> propagateAction >>> launchAff_)
        Cancelable.subscribe (eventStream KeyboardEvent.fromEvent (EventType "keypress")) \keyEvent -> case KeyboardEvent.key keyEvent of
            "Delete" -> launchAff_ $ propagateAction (KeyboardAction DeleteKey)
            _ -> pure unit
        Cancelable.subscribe initial.foreignActions \action -> launchAff_ $ propagateAction $ ForeignAction action

    handleAction :: EditorAction -> TeaM EditorState EditorGeometryId EditorAction Unit
    handleAction = case _ of
        ForeignAction (CreateBranch path pattern) -> createBranch path pattern
        ForeignAction (AddNode name argumentCount) -> createNode { name, argumentCount }
        ForeignAction (EditBranch name id) -> editBranch (name /\ id)
        ForeignAction (DeleteBranch name id) -> deleteBranch (name /\ id)
        ForeignAction (TogglePointerEvents shouldGetEnabled) -> assign _pointerEventsEnabled shouldGetEnabled
        KeyboardAction DeleteKey -> usesPointerEvents do
            use _selection >>= case _ of
                SelectedNode id -> do
                    success <- deleteNode id
                    when success do
                        assign _selection NoSelection
                _ -> pure unit
        NodeAction (SelectNode event path) -> usesPointerEvents do
            rememberMousePosition event
            let nodeId = NonEmptyArray.head path
            selectNode nodeId
            case NonEmptyArray.index path 1 of
                Nothing -> pure unit
                Just parent -> selectNestedNode { parent, nodeId }
            -- We do not want to select more than one thing at once
            stopPropagation
        NodeAction (SelectPin event pinId path) -> usesPointerEvents do
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
        MouseUp event -> usesPointerEvents do
            handleAction $ RefreshSelection event
        DeleteConnection from to -> usesPointerEvents do
            deleteConnection from to
        MouseMove event -> usesPointerEvents do
            oldPosition <- get <#> view _mousePosition
            rememberMousePosition event

            handleAction $ RefreshSelection event
            updateHovered

            get <#> preview (_selection <<< _selectedNode) >>= traverse_ \id -> do
                    let delta = event.worldPosition - oldPosition
                    modifying (_visualRule <<< VisualGraph._ruleNodes <<< _atHashMap id <<< _Just <<< VisualGraph._patternNode <<< _position) ((+) delta)

    render :: Stream.Discrete EditorState -> Stream.Discrete (MultiStepRenderer _ _)
    render state = state <#> preview _rule <#> isJust # Aged.dropDuplicates # flip Stream.bind case _ of
        false -> pure $ (0 /\ Geometry.None zero) /\ []
        true -> ado
            step1 <- step1 state
            step2 <- connections state
            step3 <- connectionPreview state
            in (1 /\ step1) /\ 
                [ step2 >>> Tuple 0
                , step3 >>> Tuple 2
                ]
        
    step1 :: Stream.Discrete EditorState -> Stream.Discrete (Geometry _ _)
    step1 state = state 
            # previewMap _rule
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
                        , headId: state 
                            # previewMap _ruleHead
                            # Aged.dropDuplicates
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
connections state = ado
    connections <- state
        # previewMap _ruleConnections
        # Aged.dropDuplicates
        <#> BiHashMap.connections
    hoveredConnection <- state
        <#> preview _hoveredConnection
        # Aged.dropDuplicatesOn _Just
    in \report -> Geometry.group
        { children: connections <#> 
            \(from /\ to) -> do
                let isHoveredOver = hoveredConnection == Just (from /\ to)
                let connectionWeight = if isHoveredOver then Opt.opt hoveredConnectionWeight else Opt.undefined
                let connection = renderConnection
                      connectionWeight
                      (lookupBothPinSides report from) 
                      (lookupBothPinSides report to)

                Geometry.reporter 
                    { id: ConnectionGeometry from to
                    , reportAbsoluteBounds: true
                    , reportTransform: true
                    , reportGeometry: true
                    , target: Geometry.group 
                        { children: [connection]
                        , onMousedown: const $ DeleteConnection from to
                        }
                    }
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
        Opt.undefined
        (selectedPin >>= lookupBothPinSides report)
        (Just (mousePosition /\ mousePosition))

renderConnection :: Opt Number -> Maybe (Vec2 /\ Vec2) -> Maybe (Vec2 /\ Vec2) -> Geometry EditorGeometryId EditorAction
renderConnection connectionWeight left right = 
        case start, lift3 pickSide right middleRight (map x start) of
            Just from, Just to -> connection { from, to, connectionWeight } 
            _, _ -> Geometry.None zero
        where
        start = lift3 pickSide left middleLeft middleRight
        
        middleLeft = middle <$> left
        middleRight = middle <$> right

        middle (left /\ right) = (x left + x right) / 2.0

        pickSide (left /\ right) thisMiddle otherMiddle = if otherMiddle > thisMiddle then right else left

    