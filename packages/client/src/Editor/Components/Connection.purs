module Loglude.Editor.Components.Connection where

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem as Opt
import Geometry (Geometry, Vec2)
import Geometry as Geometry
import Loglude.Editor.Settings (connectionWeight, pinColor)
import Lunarlog.Editor.Types (EditorAction, EditorGeometryId)

---------- Types
type ConnectionArguments = 
    { from :: Vec2
    , to :: Vec2
    , connectionWeight :: Opt Number
    }

---------- Implementation
connection :: ConnectionArguments -> Geometry EditorGeometryId EditorAction
connection attributes@{ from, to } = Geometry.line
    { from
    , to
    , weight: Opt.fromOpt connectionWeight attributes.connectionWeight
    , stroke: pinColor
    }