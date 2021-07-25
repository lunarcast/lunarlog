module Loglude.Editor.Actions where

import Loglude

import Data.Array as Array
import Data.HashSet as HashSet
import Data.Lens (traversed)
import Data.Vec as Vec
import Geometry (CanvasMouseEvent, _position)
import Geometry.Tea (TeaM, absoluteBounds, awaitRerender, currentlyHovered)
import Loglude.Run.ExternalState (assign, modifying, use)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction, EditorGeometryId(..), EditorState, Selection(..), _atRuleNode, _atVisualRuleNode, _hovered, _mousePosition, _ruleBody, _ruleNode, _selection, _visualRuleNode, freshNode, freshPin, selectionToId)

---------- Types
type ClientM = TeaM EditorState EditorGeometryId EditorAction

---------- Implementation
updateHovered :: ClientM Unit
updateHovered = do
    mousePosition <- use _mousePosition
    selection <- use _selection
    let except = maybe HashSet.empty HashSet.singleton $ selectionToId selection
    hoverStack <- currentlyHovered except mousePosition
    assign _hovered hoverStack

selectNode :: NodeId -> ClientM Unit
selectNode nodeId = do
    assign _selection $ SelectedNode nodeId

    -- Mark the grabbed pattern as top-level
    modifying _ruleBody $ Array.delete nodeId >>> flip Array.snoc nodeId

-- | Remove all data about a pin from the state
deletePin :: PinId /\ NodeId -> ClientM Unit
deletePin (pinId /\ pinNodeId) = do
    assign (_atRuleNode pinNodeId) Nothing

dropPattern :: NodeId -> ClientM Unit
dropPattern nodeId = do
    use _hovered >>= case _ of
        stack | [NestedPinDropZone id, NodeGeometry pinNodeId, NodeGeometry parent] <- Array.take 3 stack -> do
            -- deletePin pinNodeId

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