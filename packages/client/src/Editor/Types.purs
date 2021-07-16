module Lunarlog.Editor.Types where

import Loglude

import Data.Aged (Aged)
import Data.Aged as Aged
import Data.Array.NonEmpty as NonEmptyArray
import Data.Natural (Natural, natToInt)
import FRP.Stream as Stream
import Geometry (Vec2, CanvasMouseEvent)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId)
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Types
data PatternAction
    = SelectNode (NonEmptyArray NodeId)

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
patternActionWithParent parent (SelectNode path) = SelectNode (path `NonEmptyArray.snoc` parent)

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

_visualRule :: Lens' EditorState VisualGraph.Rule
_visualRule = prop (Proxy :: _ "visualRule") <<< Aged._aged

_mousePosition :: Lens' EditorState Vec2
_mousePosition = prop (Proxy :: _ "mousePosition")