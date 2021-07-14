module Lunarlog.Editor.Types where

import Loglude

import Data.Aged (Aged)
import Data.Aged as Aged
import FRP.Stream as Stream
import Geometry (Vec2, CanvasMouseEvent)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId)
import Lunarlog.Core.NodeGraph as NodeGraph

---------- Types
data PatternAction
    = ClickedPin PinId
    | SelectNode
    | NestedPatternAction NodeId PatternAction

data EditorAction
    = NodeAction NodeId PatternAction
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
    }

---------- Lenses
_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_rule :: Lens' EditorState NodeGraph.Rule
_rule = prop (Proxy :: _ "rule") <<< Aged._aged

_visualRule :: Lens' EditorState VisualGraph.Rule
_visualRule = prop (Proxy :: _ "visualRule") <<< Aged._aged

_mousePosition :: Lens' EditorState Vec2
_mousePosition = prop (Proxy :: _ "mousePosition")