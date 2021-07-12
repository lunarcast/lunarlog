module Geometry 
    ( module Geometry.Base
    , module Geometry.Tea
    , module Geometry.TextBaseline
    , module Geometry.Transform
    , module Geometry.Vector
    , module Geometry.Shapes.Padding
    , module Graphics.Canvas
    ) where

import Geometry.Base

import Geometry.Shapes.Padding (Padding, PaddingPlacement, aabbPadding, equalPadding, fourWayPadding, xyPadding)
import Geometry.Tea (SetupArgs, Tea, launchTea)
import Geometry.TextBaseline (TextBaseline)
import Geometry.Transform (TransformMatrix(..))
import Geometry.Vector (Axis(..), Vec2, _axis, _insideVector, _otherAxis, _x, _y, bimapAxis, buildFromAxis, distance, distanceSquared, fromTuple, indexByAxis, lmapAxis, mapAxis, other, rmapAxis, toTuple, x, y)
import Graphics.Canvas (Context2D)