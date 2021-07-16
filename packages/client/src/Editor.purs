module Lunarlog.Editor where

import Loglude

import Control.Plus (empty)
import Data.Aged (aged)
import Data.Aged as Aged
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.HashMap as HashMap
import Data.Lens (_Just)
import Data.MouseButton (nothingPressed)
import Data.Traversable (sequence)
import Data.Typelevel.Undefined (undefined)
import Debug (spy, traceM)
import Geometry (Geometry(..), Tea)
import Geometry as Geometry
import Geometry.Base (mapAction)
import Geometry.Tea (TeaM, createMouseEvent, eventStream)
import Graphics.Canvas (Context2D)
import Loglude.Cancelable as Cancelable
import Loglude.Data.Lens (_atHashMap)
import Loglude.ReactiveRef (writeable)
import Loglude.ReactiveRef as RR
import Lunarlog.Client.VisualGraph.Render (renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction(..), EditorState, PatternAction(..), Selection(..), _mousePosition, _rule, _selection, _visualRule, freshNode, freshPin)
import Prelude (when, zero)
import Run.State (get, modify)
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
        , NodeId (id + 1) /\ NodeGraph.Unify (NodeGraph.PinId 0)
        , NodeId (id + 2) /\ NodeGraph.Unify (NodeGraph.PinId 1)
        , NodeId (id + 3) /\ NodeGraph.PatternNode
            { name: "Tuple"
            , arguments:  NodeId <$> [id + 4, id + 5]
            }
        , NodeId (id + 4) /\ NodeGraph.PatternNode
            { name: "Zero"
            , arguments: []
            }
        , NodeId (id + 5) /\ NodeGraph.Unify (NodeGraph.PinId 2)
        ] 

myVisualPattern :: Effect (VisualGraph.Rule)
myVisualPattern = do
    node1 <- { position: _ } <$> writeable (Aged.aged $ vec2 100.0 200.0)
    node2 <- { position: _ } <$> writeable (Aged.aged $ vec2 400.0 200.0)
    pure
        { connections: HashMap.empty
        , nodes: HashMap.fromArray
            [ NodeId 0 /\ VisualGraph.PatternNode node1
            , NodeId 6 /\ VisualGraph.PatternNode node2
            ]
        }

---------- Implementation
scene :: Ask Context2D => VisualGraph.Rule -> Tea EditorState EditorAction
scene visualRule =
    { initialState: 
        { visualRule: aged visualRule
        , rule: aged rule
        , selection: NoSelection
        , mouseMove: empty
        , mousePosition: zero
        , nextId: zero
        }
    , render
    , handleAction
    , setup
    }
    where
    setup { propagateAction } = do
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mousemove) $ (createMouseEvent >>> MouseMove >>> propagateAction)
        Cancelable.subscribe (eventStream MouseEvent.fromEvent EventTypes.mouseup) $ (createMouseEvent >>> MouseUp >>> propagateAction)

    handleAction :: EditorAction -> TeaM EditorState EditorAction Unit
    handleAction = case _ of
        NodeAction (SelectNode path) -> do
            let nodeId = NonEmptyArray.head path
            modify $ set _selection $ SelectedNode nodeId
            case NonEmptyArray.index path 1 of
                Nothing -> pure unit
                Just parent -> do
                    newPin <- freshPin
                    newPinNode <- freshNode
                    traceM { parent, nodeId, path, newPin, newPinNode }
                    modify $ set ( _rule <<< NodeGraph._ruleNodes <<< _atHashMap newPinNode) $ Just $ NodeGraph.Unify newPin
                    modify $ over 
                        ( _rule 
                        <<< NodeGraph._ruleNodes 
                        <<< _atHashMap parent 
                        <<< _Just 
                        <<< NodeGraph._patternNode 
                        <<< NodeGraph._patternArguments
                        ) $ map \id -> if id == nodeId then newPinNode else id
        RefreshSelection event -> do
            when (nothingPressed event.buttons) do
                modify $ set _selection NoSelection
        MouseUp event -> do
            handleAction $ RefreshSelection event
        MouseMove event -> do
            oldPosition <- get <#> view _mousePosition
            modify $ set _mousePosition $ event.worldPosition

            handleAction $ RefreshSelection event

            get <#> view _selection >>= case _ of
                NoSelection -> pure unit
                SelectedNode id -> do
                    let delta = event.worldPosition - oldPosition
                    get <#> preview (_visualRule <<< VisualGraph._ruleNodes <<< _atHashMap id <<< _Just <<< VisualGraph._patternNode)
                        >>= case _ of
                            Nothing -> pure unit
                            Just pattern -> do
                                liftEffect $ RR.modify (over Aged._aged $ (+) delta) pattern.position

    render :: Ask Context2D => ReadableRef EditorState -> ReadableRef (Geometry _)
    render state = do
        state 
            <#> _.rule
            # RR.dropDuplicates
            <#> view NodeGraph._ruleBody &&& view NodeGraph._ruleNodes
            >>= \(bodyNodes /\ nodes) -> do
                let asArray 
                      = bodyNodes 
                      <#> identity &&& flip HashMap.lookup nodes -- NodeId -> NodeId /\ Maybe Node
                      <#> uncurry (Tuple >>> map) -- a /\ Maybe b -> Maybe (a /\ b)
                      # Array.catMaybes
                let 
                  renderNode :: NodeId /\ NodeGraph.Node -> _
                  renderNode (nodeId /\ (NodeGraph.Unify _)) = undefined
                  renderNode (nodeId /\ (NodeGraph.PatternNode pattern)) = do
                    visualPattern <- visualPatternStream 
                    case visualPattern of
                        Just visualPattern -> ado
                            geometry <- renderPattern 
                                { lookupPattern: flip HashMap.lookup nodes
                                , visualPattern
                                , pattern
                                , nodeId
                                }
                            in mapAction NodeAction geometry
                        -- TODO: better error handling
                        Nothing -> spy "aaaaaa" $ pure $ None zero
                    where
                    visualPatternStream 
                        = state
                        <#> _.visualRule 
                        # RR.dropDuplicates 
                        <#> preview (VisualGraph._ruleNodes <<< _atHashMap nodeId <<< _Just <<< VisualGraph._patternNode)
                asArray
                    # map renderNode 
                    # sequence
                    # map (\children -> Geometry.group { children })