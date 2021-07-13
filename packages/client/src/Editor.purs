module Lunarlog.Editor where

import Loglude

import Control.Plus (empty)
import Data.Aged (Aged, aged)
import Data.Aged as Aged
import Data.MouseButton (nothingPressed)
import Effect.Class.Console (log)
import FRP.Stream as Stream
import Geometry (Geometry, Tea, Vec2)
import Geometry.Tea (createMouseEvent, eventStream)
import Graphics.Canvas (Context2D)
import Loglude.Cancelable as Cancelable
import Loglude.ReactiveRef (writeable)
import Loglude.ReactiveRef as RR
import Lunarlog.Client.VisualGraph.Render (PatternAction(..), renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Prelude (when)
import Run.State (get, modify)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

---------- Types
type MyAction = PatternAction

data Selection
    = SelectedNode NodeId
    | NoSelection

type EditorState = 
    { pattern :: Aged VisualGraph.Pattern
    , nestedPattern :: Aged NodeGraph.Pattern
    , selection :: Selection
    , mouseMove :: Stream.Discrete MouseEvent
    , mousePosition :: Vec2 }

---------- Selection
_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_pattern :: Lens' EditorState VisualGraph.Pattern
_pattern = prop (Proxy :: _ "pattern") <<< Aged._aged

_mousePosition :: Lens' EditorState Vec2
_mousePosition = prop (Proxy :: _ "mousePosition")

---------- Constants
myPattern :: NodeGraph.Pattern
myPattern =
    { name: "Example pattern"
    , arguments: 
        [ NodeGraph.Pin $ NodeGraph.PinId 0
        , NodeGraph.Pin $ NodeGraph.PinId 1
        , NodeGraph.NestedPattern
            { name: "Tuple"
            , arguments: 
                [ NodeGraph.NestedPattern
                    { name: "Zero"
                    , arguments: []
                    }
                , NodeGraph.Pin $ NodeGraph.PinId 2
                ] 
            }
        ] 
    }

myVisualPattern :: Effect (VisualGraph.Pattern)
myVisualPattern = { position: _ } <$> writeable (Aged.aged $ vec2 100.0 200.0)

---------- Implementation
scene :: Ask Context2D => VisualGraph.Pattern -> Tea EditorState MyAction
scene pattern =
    { initialState: 
        { pattern: aged pattern
        , nestedPattern: aged myPattern
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
    handleAction = case _ of
        ClickedPin (PinId id) -> do
            log $ "Clicked!!! " <> show id 
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
                SelectedNode _ -> do
                    let delta = event.worldPosition - oldPosition
                    liftEffect $ RR.modify (over Aged._aged $ (+) delta) pattern.position
        SelectNode nodeId -> do
            modify $ set _selection $ SelectedNode nodeId
            log $ "Selected " <> show nodeId

    render :: Ask Context2D => ReadableRef _ -> ReadableRef (Geometry _)
    render state = do
        let myPatternStream = state <#> _.nestedPattern # RR.dropDuplicates
        pattern <- state <#> _.pattern # RR.dropDuplicates
        renderPattern pattern myPatternStream