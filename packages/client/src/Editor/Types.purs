module Lunarlog.Editor.Types where

import Loglude

import Data.Array.NonEmpty as NonEmptyArray
import FRP.Stream as Stream
import Geometry (Vec2, CanvasMouseEvent)
import Loglude.Run.ExternalState (EXTERNAL_STATE, modifying, use)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Types
data PatternAction
    = SelectNode CanvasMouseEvent (NonEmptyArray NodeId)

-- | Different ids we can use to query the bounds of the geometry tree
data EditorGeometryId
    = NodeGeometry NodeId
    | NestedPinDropZone PinId

data EditorAction
    = NodeAction PatternAction
    | RefreshSelection CanvasMouseEvent
    | MouseMove CanvasMouseEvent
    | MouseUp CanvasMouseEvent

data Selection
    = SelectedNode NodeId
    | NoSelection

data HoverTarget
    = HoveredPin PinId
    | HoveredPinDropZone PinId
    | NothingHovered

type EditorState = 
    { rule :: NodeGraph.Rule
    , visualRule :: VisualGraph.Rule
    , selection :: Selection
    , hovered :: HoverTarget
    , mouseMove :: Stream.Discrete MouseEvent
    , mousePosition :: Vec2
    , nextId :: Natural
    }

---------- Helpers
patternActionWithParent :: NodeId -> PatternAction -> PatternAction
patternActionWithParent parent (SelectNode event path) = SelectNode event (path `NonEmptyArray.snoc` parent)

fresh :: forall r. Run (EXTERNAL_STATE EditorState r) Natural
fresh = use _nextId <* modifying _nextId succ 

freshPin :: forall r. Run (EXTERNAL_STATE EditorState r) NodeGraph.PinId
freshPin = fresh <#> natToInt <#> NodeGraph.PinId

freshNode :: forall r. Run (EXTERNAL_STATE EditorState r) NodeGraph.NodeId
freshNode = fresh <#> natToInt <#> NodeGraph.NodeId

selectionIsNode :: Selection -> Boolean
selectionIsNode (SelectedNode _) = true
selectionIsNode _ = false 

idToHovered :: EditorGeometryId -> HoverTarget
idToHovered (NestedPinDropZone id) = HoveredPinDropZone id
idToHovered _ = NothingHovered

selectionToId :: Selection -> Maybe EditorGeometryId
selectionToId (SelectedNode id) = Just $ NodeGeometry id
selectionToId NoSelection = Nothing

---------- Lenses
_nextId :: Lens' EditorState Natural
_nextId = prop (Proxy :: _ "nextId")

_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_selectedNode :: Prism' Selection NodeId
_selectedNode = prism' SelectedNode case _ of
    SelectedNode id -> Just id
    _ -> Nothing

_rule :: Lens' EditorState NodeGraph.Rule
_rule = prop (Proxy :: _ "rule")

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

_hoveredPinDropZone :: Prism' HoverTarget PinId
_hoveredPinDropZone = prism' HoveredPinDropZone case _ of
    HoveredPinDropZone id -> Just id
    _ -> Nothing

---------- Typeclass isntances
derive instance Eq EditorGeometryId
derive instance Eq HoverTarget

derive instance Generic EditorGeometryId _
derive instance Generic HoverTarget _

instance Debug EditorGeometryId where
    debug = genericDebug

instance Debug HoverTarget where
    debug = genericDebug

instance Hashable EditorGeometryId where
    hash = hash <<< case _ of
        NodeGeometry id -> Left id
        NestedPinDropZone id -> Right id

instance Hashable HoverTarget where
    hash = hash <<< case _ of
        HoveredPin id -> Left $ Left id
        HoveredPinDropZone id -> Left $ Right id
        NothingHovered -> Right unit