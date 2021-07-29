module Loglude.Editor.Actions where

import Loglude

import Data.Array as Array
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.Vec as Vec
import Debug (traceM)
import Geometry (CanvasMouseEvent, Context2D, _position)
import Geometry.Tea (TeaM, absoluteBounds, awaitRerender, currentlyHovered)
import Loglude.Data.BiHashMap as BiHashMap
import Loglude.Run.ExternalState (EXTERNAL_STATE, assign, get, gets, modifying, preuse, put, runFocused, use)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId(..), PinId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (BranchPath, EditorAction, EditorGeometryId(..), EditorState, PatternShape, Selection(..), _atBranch, _atRuleConnection, _atRuleConnectionPair, _atRuleNode, _atVisualRuleNode, _currentRule, _hovered, _mousePosition, _nextId, _ruleBody, _ruleHead, _ruleNode, _ruleNodes, _selection, _visualRuleNode, freshNode, freshPin, selectionToNodeId)

---------- Types
type ClientM = TeaM EditorState EditorGeometryId EditorAction

---------- Implementation
constructNode :: forall r. PatternShape -> Run (EXTERNAL_STATE Natural r) (HashMap NodeId NodeGraph.Node)
constructNode{ name, argumentCount }  = do
    startingId <- get <#> natToInt
    put (intToNat $ startingId + 2 * argumentCount + 1)
    pure $ go startingId
    where
    go startingId = HashMap.fromArray $ Array.cons (NodeId startingId /\ node) pins 
        where
        node = NodeGraph.PatternNode
            { name
            , arguments: indices <#> \index-> NodeId (startingId + index)
            }

        pins = indices <#> \index -> NodeId (startingId + index) /\ NodeGraph.Unify (NodeGraph.PinId $ startingId + argumentCount + index)

        nextId = startingId + argumentCount + 1
        indices = Array.replicate argumentCount unit # Array.mapWithIndex (((+) 1) >>> const)

-- | Construct a rule containing no data
emptyRule :: forall r. PatternShape -> Run (EXTERNAL_STATE Natural r) NodeGraph.Rule
emptyRule pattern = ado
    startingId <- get <#> natToInt
    nodes <- constructNode pattern
    in NodeGraph.Rule
        { head: NodeId startingId
        , body: [NodeId startingId]
        , connections: BiHashMap.empty
        , nodes
        }

updateHovered :: Ask Context2D => ClientM Unit
updateHovered = do
    mousePosition <- use _mousePosition
    selection <- use _selection
    let except = maybe HashSet.empty HashSet.singleton $ selectionToNodeId selection
    hoverStack <- currentlyHovered except mousePosition
    assign _hovered hoverStack

-- | Move a node on top of all the others
toTop :: NodeId -> ClientM Unit
toTop nodeId = modifying _ruleBody $ Array.delete nodeId >>> flip Array.snoc nodeId 

-- | Run when the user clicks a node
selectNode :: NodeId -> ClientM Unit
selectNode nodeId = do
    assign _selection $ SelectedNode nodeId

    -- Mark the grabbed pattern as top-level
    toTop nodeId

-- | Run when the user clicks a pin
selectPin :: PinId -> NodeId -> ClientM Unit
selectPin pinId topmostNodeId = do
    use _selection >>= case _ of
        SelectedPin selected | selected /= pinId -> do
            assign (_atRuleConnectionPair selected) $ Just pinId
            assign _selection NoSelection
        _ -> do
            assign _selection $ SelectedPin pinId
            toTop topmostNodeId

-- | Remove all data about a pin from the state
deletePin :: PinId /\ NodeId -> ClientM Unit
deletePin (pinId /\ pinNodeId) = do
    assign (_atRuleNode pinNodeId) Nothing
    assign (_atRuleConnectionPair pinId) Nothing

-- | Remove all data about a node from the state. 
-- | Does nothing on head nodes
deleteNode :: NodeId -> ClientM Boolean
deleteNode nodeId = do
    headNode <- preuse _ruleHead
    if Just nodeId /= headNode then do
        preuse (_ruleNode nodeId) >>= traverse_ case _ of
            NodeGraph.Unify pinId -> deletePin (pinId /\ nodeId)
            NodeGraph.PatternNode { arguments } -> for_ arguments deleteNode
        assign (_atRuleNode nodeId) Nothing
        assign (_atVisualRuleNode nodeId) Nothing
        modifying _ruleBody $ Array.delete nodeId
        pure true
    else
        pure false

createVisualPattern :: NodeId -> ClientM Unit
createVisualPattern nodeId = assign (_atVisualRuleNode nodeId) $ Just $ VisualGraph.PatternNode { position: vec2 200.0 200.0 }

-- | Create an empty branch
createBranch :: BranchPath -> PatternShape -> ClientM Unit
createBranch path pattern = do
    nodeId <- gets (_.nextId >>> natToInt >>> NodeId)
    newRule <- runFocused _nextId $ emptyRule pattern 

    assign (_atBranch path) $ Just newRule
    createVisualPattern nodeId

-- | Create an empty node
createNode :: PatternShape -> ClientM Unit
createNode pattern = do
    nodeId <- gets (_.nextId >>> natToInt >>> NodeId)
    nodes <- runFocused _nextId $ constructNode pattern 

    -- Mark the node itself for diplay
    modifying _ruleNodes $ HashMap.union nodes
    createVisualPattern nodeId

    toTop nodeId

editBranch :: String /\ Int -> ClientM Unit
editBranch newPath = do
    assign _currentRule $ Just newPath

    assign _selection NoSelection
    assign _hovered []


dropPattern :: NodeId -> ClientM Unit
dropPattern nodeId = do
    headNode <- preuse _ruleHead
    unless (Just nodeId == headNode) $ use _hovered >>= case _ of
        stack | [NestedPinDropZone id, NodeGeometry pinNodeId, NodeGeometry parent] <- Array.take 3 stack -> do
            deletePin (id /\ pinNodeId)

            -- Place dropped node inside the parent of the pin
            modifying (_ruleNode parent <<< NodeGraph._patternNode <<< NodeGraph._patternArguments) $ 
                map \child -> if child == pinNodeId 
                    then nodeId
                    else child

            -- Remove visual data (eg: position) of the dropped node
            assign (_atVisualRuleNode nodeId) Nothing

            -- Remove node from rule body
            modifying _ruleBody $ Array.delete nodeId
        _ -> pure unit

-- | Update the last mouse position in the state
rememberMousePosition :: CanvasMouseEvent -> ClientM Unit
rememberMousePosition event = assign _mousePosition $ event.worldPosition

-- | Run when the user clicks on a connection
deleteConnection :: PinId -> PinId -> ClientM Unit
deleteConnection from to = do
    assign (_atRuleConnection from to) false

-- | Respond to a user clicking on a nested node
selectNestedNode :: { parent :: NodeId, nodeId :: NodeId } -> ClientM Unit
selectNestedNode { parent, nodeId } = do
    newPin <- freshPin
    newPinNode <- freshNode
    absoluteBounds (NodeGeometry nodeId) >>= traverse_ \bounds -> do
        -- Create replacement pin
        assign (_atRuleNode newPinNode) $ Just $ NodeGraph.Unify newPin

        -- Create visual node for grabbed pattern
        assign (_atVisualRuleNode nodeId) $ Just $ VisualGraph.PatternNode { position: bounds.position }

        -- Remove the grabbed pattern from the argument list of the parent
        modifying (_ruleNode parent <<< NodeGraph._patternNode <<< NodeGraph._patternArguments <<< traversed) 
            \id -> if id == nodeId then newPinNode else id

        -- Trigger rerender for the grabbed pattern to resize
        awaitRerender

        -- Move the resized pattern to look "good" relative to the mouse
        absoluteBounds (NodeGeometry nodeId) >>= traverse_ \bounds' -> do
            mousePosition <- use _mousePosition
            let relativeMousePosition = mousePosition - bounds.position

            -- size' * mouse / size
            let fixedRelativeMousePosition = Vec.zipWith (/) (Vec.zipWith (*) bounds'.size relativeMousePosition) bounds.size
            let newPosition = mousePosition - fixedRelativeMousePosition

            assign (_visualRuleNode nodeId <<< VisualGraph._patternNode <<< _position) newPosition