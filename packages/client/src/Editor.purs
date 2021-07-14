module Lunarlog.Editor where

import Loglude

import Control.Plus (empty)
import Data.Aged (aged)
import Data.Aged as Aged
import Data.HashMap as HashMap
import Data.Lens (_Just)
import Data.MouseButton (nothingPressed)
import Data.Traversable (sequence)
import Data.Typelevel.Undefined (undefined)
import Effect.Class.Console (log)
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
import Lunarlog.Core.NodeGraph (NodeId(..), PinId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction(..), EditorState, PatternAction(..), Selection(..), _mousePosition, _selection, _visualRule)
import Prelude (when)
import Run.State (get, modify)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

---------- Constants
rule :: NodeGraph.Rule
rule = NodeGraph.Rule
    { head: NodeId 0
    , nodes: HashMap.fromArray
        [ NodeId 0 /\  mkNode 0]
    , connections: HashMap.empty
    }
    where
    mkNode id = NodeGraph.PatternNode
        { name: "Example pattern"
        , id: NodeId id
        , arguments: 
            [ NodeGraph.Pin $ NodeGraph.PinId 0
            , NodeGraph.Pin $ NodeGraph.PinId 1
            , NodeGraph.NestedPattern
                { name: "Tuple"
                , id: NodeId $ id + 1
                , arguments: 
                    [ NodeGraph.NestedPattern
                        { name: "Zero"
                        , id: NodeId $ id + 2
                        , arguments: []
                        }
                    , NodeGraph.Pin $ NodeGraph.PinId 2
                    ] 
                }
            ] 
        }

myVisualPattern :: Effect (VisualGraph.Rule)
myVisualPattern = do
    node <- { position: _ } <$> writeable (Aged.aged $ vec2 100.0 200.0)
    pure
        { connections: HashMap.empty
        , nodes: HashMap.fromArray
            [ NodeId 0 /\ VisualGraph.PatternNode node ]
        }

---------- Implementation
scene :: Ask Context2D => VisualGraph.Rule -> Tea EditorState EditorAction
scene visualRule =
    { initialState: 
        { visualRule: aged visualRule
        , rule: aged rule
        , selection: NoSelection
        , mouseMove: empty
        , mousePosition: zero }
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
        NodeAction nodeId (ClickedPin (PinId id)) -> do
            log $ "Clicked!!! " <> show id 
        NodeAction nodeId SelectNode -> do
            modify $ set _selection $ SelectedNode nodeId
            log $ "Selected " <> show nodeId
        NodeAction _ (NestedPatternAction nodeId action) -> do
            handleAction $ NodeAction nodeId action
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
                        >>= traverse_ \pattern -> do
                            liftEffect $ RR.modify (over Aged._aged $ (+) delta) pattern.position

    render :: Ask Context2D => ReadableRef EditorState -> ReadableRef (Geometry _)
    render state = do
        state 
            <#> _.rule 
            # RR.dropDuplicates
            <#> view NodeGraph._ruleNodes
            >>= \nodes -> do
                let asArray = HashMap.toArrayBy Tuple nodes
                let 
                  renderNode :: NodeId /\ NodeGraph.Node -> _
                  renderNode (nodeId /\ (NodeGraph.Unify _)) = undefined
                  renderNode (nodeId /\ (NodeGraph.PatternNode pattern)) = do
                    visualPattern <- visualPatternStream 
                    case visualPattern of
                        Just visualPattern -> ado
                            geometry <- renderPattern visualPattern pattern
                            in mapAction (NodeAction nodeId) geometry
                        -- TODO: better error handling
                        Nothing -> pure $ None zero
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