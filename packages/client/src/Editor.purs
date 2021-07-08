module Lunarlog.Editor where

import Loglude

import Control.Plus (empty)
import Debug (spy)
import Effect.Class.Console (log)
import FRP.Stream as Stream
import Geometry (Geometry, Tea)
import Geometry as Geometry
import Graphics.Canvas (Context2D)
import Loglude.ReactiveRef (writeable)
import Loglude.ReactiveRef as RR
import Lunarlog.Client.VisualGraph.Render (PatternAction(..), renderPattern)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId, PinId(..))
import Lunarlog.Core.NodeGraph as NodeGraph
import Run.State (get, modify)

---------- Types
type MyAction = PatternAction

data Selection
    = SelectedNode NodeId
    | NoSelection

type EditorState = 
    { pattern :: VisualGraph.Pattern
    , selection :: Selection
    , mouseMove :: Stream.Discrete MouseEvent }

---------- Selection
_selection :: Lens' EditorState Selection
_selection = prop (Proxy :: _ "selection")

_pattern :: Lens' EditorState VisualGraph.Pattern
_pattern = prop (Proxy :: _ "pattern")

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

myVisualPattern :: Cancelable (VisualGraph.Pattern)
myVisualPattern = { position: _ } <$> writeable (vec2 100.0 200.0)

---------- Implementation
scene :: Context2D -> VisualGraph.Pattern -> Tea EditorState MyAction
scene context pattern = 
    { context
    , initialState: { pattern, selection: NoSelection, mouseMove: empty }
    , render
    , handleAction
    , setup
    }
    where
    setup = const $ pure unit
    handleAction = case _ of
        ClickedPin (PinId id) -> do
            log $ "Clicked!!! " <> show id 
            -- when (even id) stopPropagation
        SelectNode nodeId -> do
            modify $ set _selection $ SelectedNode nodeId
            { position } <- get <#> view _pattern 
            currentPosition <- liftEffect $ RR.read position.ref
            liftEffect $ RR.write position (currentPosition + vec2 20.0 20.0)
            log $ "Selected " <> show nodeId

    render :: Ask Context2D => _ -> Geometry _
    render { pattern } = spy "geometry" $ Geometry.group { children: [geom], onClick: const $ ClickedPin $ PinId 700 }
        where
        geom = renderPattern pattern myPattern
