module Lunarlog.Editor.Types where

import Loglude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Lens.Index (ix)
import Geometry (Vec2, CanvasMouseEvent)
import Loglude.Data.BiHashMap (_atBiHashMap, _atBiHashMapConnection)
import Loglude.Run.ExternalState (EXTERNAL_STATE, modifying, use)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Types
data PatternAction
    = SelectNode CanvasMouseEvent (NonEmptyArray NodeId)
    | SelectPin CanvasMouseEvent PinId (Array NodeId)

-- | A pin can either be on the left or the right side of a node
data PinSide = LeftPin | RightPin

-- | Different ids we can use to query the bounds of the geometry tree
data EditorGeometryId
    = NodeGeometry NodeId
    | NestedPinDropZone PinId
    | PinGeometry PinId PinSide
    | ConnectionGeometry PinId PinId

data EditorAction
    = NodeAction PatternAction
    | RefreshSelection CanvasMouseEvent
    | MouseMove CanvasMouseEvent
    | MouseUp CanvasMouseEvent
    | DeleteConnection PinId PinId

data Selection
    = SelectedNode NodeId
    | SelectedPin PinId
    | NoSelection

type HoverTarget = Array EditorGeometryId

type EditorState = 
    { rule :: NodeGraph.Rule
    , visualRule :: VisualGraph.Rule
    , selection :: Selection
    , hovered :: HoverTarget
    , mousePosition :: Vec2
    , nextId :: Natural
    }

---------- Helpers
patternActionWithParent :: NodeId -> PatternAction -> PatternAction
patternActionWithParent parent (SelectNode event path) = SelectNode event (path `NonEmptyArray.snoc` parent)
patternActionWithParent parent (SelectPin event pin path) = SelectPin event pin (path `Array.snoc` parent)

fresh :: forall r. Run (EXTERNAL_STATE EditorState r) Natural
fresh = use _nextId <* modifying _nextId succ 

freshPin :: forall r. Run (EXTERNAL_STATE EditorState r) NodeGraph.PinId
freshPin = fresh <#> natToInt <#> NodeGraph.PinId

freshNode :: forall r. Run (EXTERNAL_STATE EditorState r) NodeGraph.NodeId
freshNode = fresh <#> natToInt <#> NodeGraph.NodeId

selectionIsNode :: Selection -> Boolean
selectionIsNode (SelectedNode _) = true
selectionIsNode _ = false 

selectionToNodeId :: Selection -> Maybe EditorGeometryId
selectionToNodeId (SelectedNode id) = Just $ NodeGeometry id
selectionToNodeId _ = Nothing

---------- Lenses
_nextId :: Lens' EditorState Natural
_nextId = prop (Proxy :: _ "nextId")

_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_selectedNode :: Prism' Selection NodeId
_selectedNode = prism' SelectedNode case _ of
    SelectedNode id -> Just id
    _ -> Nothing

_selectedPin :: Prism' Selection PinId
_selectedPin = prism' SelectedPin case _ of
    SelectedPin id -> Just id
    _ -> Nothing

_rule :: Lens' EditorState NodeGraph.Rule
_rule = prop (Proxy :: _ "rule")

_ruleConnections :: Lens' EditorState (BiHashMap PinId)
_ruleConnections = _rule <<< NodeGraph._ruleConnections

_atRuleConnectionPair :: PinId -> Lens EditorState EditorState (HashSet PinId) (Maybe PinId)
_atRuleConnectionPair id = _rule <<< NodeGraph._ruleConnections <<< _atBiHashMap id

_atRuleConnection :: PinId -> PinId -> Lens' EditorState Boolean
_atRuleConnection from to = _rule <<< NodeGraph._ruleConnections <<< _atBiHashMapConnection from to

_ruleBody :: Lens' EditorState (Array NodeId)
_ruleBody = _rule <<< NodeGraph._ruleBody

_ruleNodes :: Lens' EditorState (HashMap NodeId NodeGraph.Node)
_ruleNodes = _rule <<< NodeGraph._ruleNodes

_atRuleNode :: NodeId -> Lens' EditorState (Maybe NodeGraph.Node)
_atRuleNode nodeId = _ruleNodes <<< _atHashMap nodeId

_ruleNode :: NodeId -> AffineTraversal' EditorState NodeGraph.Node
_ruleNode nodeId = _atRuleNode nodeId <<< _Just

_visualRuleNodes :: Lens' EditorState (HashMap NodeId VisualGraph.Node)
_visualRuleNodes = _visualRule <<< VisualGraph._ruleNodes

_atVisualRuleNode :: NodeId -> Lens' EditorState (Maybe VisualGraph.Node)
_atVisualRuleNode nodeId = _visualRuleNodes <<< _atHashMap nodeId

_visualRuleNode :: NodeId -> AffineTraversal' EditorState VisualGraph.Node
_visualRuleNode nodeId = _atVisualRuleNode nodeId <<< _Just

_visualRule :: Lens' EditorState VisualGraph.Rule
_visualRule = prop (Proxy :: _ "visualRule")

_mousePosition :: Lens' EditorState Vec2
_mousePosition = prop (Proxy :: _ "mousePosition")

_hovered :: Lens' EditorState HoverTarget
_hovered = prop (Proxy :: _ "hovered")

_connectionGeometry :: Prism' EditorGeometryId (PinId /\ PinId)
_connectionGeometry = prism' (uncurry ConnectionGeometry) case _ of
    ConnectionGeometry from to -> Just (from /\ to)
    _ -> Nothing

_hoveredConnection :: Traversal' EditorState (PinId /\ PinId)
_hoveredConnection = _hovered <<< ix 0 <<< _connectionGeometry

_nestedPinDropZone :: Prism' EditorGeometryId PinId
_nestedPinDropZone = prism' NestedPinDropZone case _ of
    NestedPinDropZone id -> Just id
    _ -> Nothing

---------- Typeclass isntances
derive instance Eq PinSide
derive instance Eq EditorGeometryId

derive instance Generic PinSide _
derive instance Generic EditorGeometryId _

instance Debug EditorGeometryId where
    debug = genericDebug

instance Debug PinSide where
    debug = genericDebug

instance Hashable PinSide where
    hash = hash <<< case _ of
        LeftPin -> false
        RightPin -> true

instance Hashable EditorGeometryId where
    hash = hash <<< case _ of
        NodeGeometry id -> Left $ Left id
        PinGeometry id side -> Left $ Right (id /\ side)
        NestedPinDropZone id -> Right $ Left id
        ConnectionGeometry from to -> Right $ Right (from /\ to)