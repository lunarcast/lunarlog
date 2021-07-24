module Lunarlog.Editor where

import Loglude

import Data.Aged as Aged
import Data.Array.NonEmpty as NonEmptyArray
import Data.HashMap as HashMap
import Data.MouseButton (nothingPressed)
import Data.Traversable (sequence)
import Effect.Aff (launchAff_)
import Geometry (Geometry, Tea)
import Geometry as Geometry
import Geometry.Base (mapAction)
import Geometry.Tea (TeaM, createMouseEvent, eventStream, stopPropagation)
import Graphics.Canvas (Context2D)
import Loglude.Cancelable as Cancelable
import Loglude.Data.Lens (_atHashMap)
import Loglude.Editor.Actions (dropPattern, selectNestedNode, selectNode, updateHovered)
import Loglude.ReactiveRef (writeable)
import Loglude.ReactiveRef as RR
import Loglude.Run.ExternalState (assign, get, use)
import Lunarlog.Client.VisualGraph.Render (renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction(..), EditorGeometryId, EditorState, HoverTarget(..), PatternAction(..), Selection(..), _hovered, _hoveredPinDropZone, _mousePosition, _ruleNode, _selection, _visualRule, _visualRuleNode)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

---------- Constants
rule :: NodeGraph.Rule
rule = NodeGraph.Rule
    { head: NodeId 0
    , body: [NodeId 0, NodeId 6]
    , nodes: HashMap.fromArray $ mkNodes "Exmaple pattern" 0 <> mkNodes "Super duper mega long name" 6
    , connections: HashMap.empty
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

myVisualPattern :: Effect (VisualGraph.Rule)
myVisualPattern = do
    node1 <- { position: _ } <$> writeable (vec2 100.0 200.0)
    node2 <- { position: _ } <$> writeable (vec2 400.0 200.0)
    pure
        { connections: HashMap.empty
        , nodes: HashMap.fromArray
            [ NodeId 0 /\ VisualGraph.PatternNode node1
            , NodeId 6 /\ VisualGraph.PatternNode node2
            ]
        }

---------- Implementation
scene :: Ask Context2D => VisualGraph.Rule -> Tea EditorState EditorGeometryId EditorAction
scene visualRule =
    { initialState: 
        { visualRule: visualRule
        , rule: rule
        , selection: NoSelection
        , hovered: NothingHovered
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
            let nodeId = NonEmptyArray.head path
            selectNode nodeId
            case NonEmptyArray.index path 1 of
                Nothing -> pure unit
                Just parent -> selectNestedNode { parent, nodeId }
            -- We do not want to select more than one thing at once
            stopPropagation
        RefreshSelection event -> do
            when (nothingPressed event.buttons) do
                use _selection >>= case _ of
                    SelectedNode id -> dropPattern id
                    _ -> pure unit
                assign _selection NoSelection
        MouseUp event -> do
            handleAction $ RefreshSelection event
        MouseMove event -> do
            oldPosition <- get <#> view _mousePosition
            assign _mousePosition $ event.worldPosition

            handleAction $ RefreshSelection event
            updateHovered

            get <#> view _selection >>= case _ of
                NoSelection -> pure unit
                SelectedNode id -> do
                    let delta = event.worldPosition - oldPosition
                    get <#> preview (_visualRule <<< VisualGraph._ruleNodes <<< _atHashMap id <<< _Just <<< VisualGraph._patternNode)
                        >>= case _ of
                            Nothing -> pure unit
                            Just pattern -> do
                                liftEffect $ RR.modify ((+) delta) pattern.position

    render :: Ask Context2D => ReadableRef EditorState -> ReadableRef (Geometry _ _)
    render state = do
        state 
            <#> _.rule
            # RR.dropDuplicates
            <#> (view NodeGraph._ruleBody &&& view NodeGraph._ruleNodes)
            >>= \(bodyNodes /\ nodes) -> do
                let 
                  renderNode :: NodeId -> _
                  renderNode nodeId = do
                    let
                      checkSelection = case _ of
                        SelectedNode selected -> selected /= nodeId 
                        _ -> false

                    geometry <- renderPattern 
                        { lookupPattern: flip HashMap.lookup nodes
                        , visualPattern: state <#> preview (_visualRuleNode nodeId <<< VisualGraph._patternNode) 
                            # RR.mapJusts Aged.dropDuplicates
                        , pattern: state <#> preview (_ruleNode nodeId <<< NodeGraph._patternNode)
                            # RR.mapJusts Aged.dropDuplicates
                        , nodeId
                        , selection: state <#> _.selection # RR.dropDuplicates
                        , hoveredPin: state <#> preview (_hovered <<< _hoveredPinDropZone)
                        }
                    pure $ mapAction NodeAction geometry
                bodyNodes
                    # map renderNode 
                    # sequence
                    # map (\children -> Geometry.group { children })