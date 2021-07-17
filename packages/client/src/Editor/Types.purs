module Lunarlog.Editor.Types where

import Loglude

import Data.Aged (Aged)
import Data.Aged as Aged
import Data.Array.NonEmpty as NonEmptyArray
import FRP.Stream as Stream
import Geometry (Vec2, CanvasMouseEvent)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId)
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Types
data PatternAction
    = SelectNode CanvasMouseEvent (NonEmptyArray NodeId)

-- | Different ids we can use to query the bounds of the geometry tree
data EditorGeometryId
    = NodeGeometry NodeId

data EditorAction
    = NodeAction PatternAction
    | RefreshSelection CanvasMouseEvent
    | MouseMove CanvasMouseEvent
    | MouseUp CanvasMouseEvent

data Selection
    = SelectedNode NodeId
    | NoSelection

type EditorState = 
    { rule :: Aged NodeGraph.Rule
    , visualRule :: Aged VisualGraph.Rule
    , selection :: Selection
    , mouseMove :: Stream.Discrete MouseEvent
    , mousePosition :: Vec2
    , nextId :: Natural
    }

---------- Helpers
patternActionWithParent :: NodeId -> PatternAction -> PatternAction
patternActionWithParent parent (SelectNode event path) = SelectNode event (path `NonEmptyArray.snoc` parent)

fresh :: forall r. Run (STATE EditorState r) Natural
fresh = use _nextId <* modifying _nextId succ 

freshPin :: forall r. Run (STATE EditorState r) NodeGraph.PinId
freshPin = fresh <#> natToInt <#> NodeGraph.PinId

freshNode :: forall r. Run (STATE EditorState r) NodeGraph.NodeId
freshNode = fresh <#> natToInt <#> NodeGraph.NodeId

---------- Lenses
_nextId :: Lens' EditorState Natural
_nextId = prop (Proxy :: _ "nextId")

_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_rule :: Lens' EditorState NodeGraph.Rule
_rule = prop (Proxy :: _ "rule") <<< Aged._aged

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
_visualRule = prop (Proxy :: _ "visualRule") <<< Aged._aged

_mousePosition :: Lens' EditorState Vec2
_mousePosition = prop (Proxy :: _ "mousePosition")

---------- Typeclass isntances
derive instance Eq EditorGeometryId

instance Hashable EditorGeometryId where
    hash id = hash case id of
        NodeGeometry id -> id