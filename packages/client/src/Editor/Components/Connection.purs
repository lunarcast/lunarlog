module Loglude.Editor.Components.Connection where

import Geometry (Geometry, Vec2)
import Geometry as Geometry
import Loglude.Editor.Settings (connectionWeight, pinColor)
import Lunarlog.Editor.Types (EditorAction, EditorGeometryId)

---------- Types
type ConnectionArguments = 
    { from :: Vec2
    , to :: Vec2
    }

---------- Implementation
connection :: ConnectionArguments -> Geometry EditorGeometryId EditorAction
connection { from, to } = Geometry.line
    { from
    , to
    , weight: connectionWeight
    , stroke: pinColor
    }