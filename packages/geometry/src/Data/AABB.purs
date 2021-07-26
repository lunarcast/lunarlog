module Geoemtry.Data.AABB where

import Loglude

import Data.Vec as Vec
import Geometry.Vector (Vec2, x, y, smallerThan, greaterThan)
import Graphics.Canvas (Rectangle)

---------- Types
type AABBLike r = ( position :: Vec2, size :: Vec2 | r )
type AABB = Record (AABBLike ())

type PointForm = { min :: Vec2, max :: Vec2 }

---------- Helpers
fromMinMax :: PointForm -> AABB
fromMinMax points =
    { position: points.min
    , size: points.max - points.min
    }

toMinMax :: AABB -> PointForm
toMinMax aabb =
    { min: aabb.position
    , max: aabb.position + aabb.size }

union :: AABB -> AABB -> AABB
union a b = fromMinMax
    { min: Vec.zipWith min a.position b.position
    , max: Vec.zipWith max (toMinMax a).max (toMinMax b).max
    }

-- | Calculate the center of some bounding box
center :: AABB -> Vec2
center { position, size } = position + map (_ / 2.0) size

-- | Calculate wether a point is inside a rect
pointInside :: Vec2 -> AABB -> Boolean
pointInside point { position, size } = point `greaterThan` position && point `smallerThan` (position + size)

-- | Calculates the bounds of a polygon. Returns Nothing if the point array is empty
fromPoints :: Array Vec2 -> Maybe AABB
fromPoints points = ado
    min <- foldr (\a b -> maybe (Just a) (Vec.zipWith min a >>> Just) b) Nothing points
    max <- foldr (\a b -> maybe (Just a) (Vec.zipWith max a >>> Just) b) Nothing points
    in fromMinMax { min, max }

-- | Get an array with all 4 points forming the aabb shape
points :: AABB -> Array Vec2
points { position, size } =
    [ position
    , vec2 (x position + x size) (y position)
    , position + size
    , vec2 (x position) (y position + y size)
    ]

-- | Convert to a rectangle renderable on the canvas
toCanvasRect :: forall r. Record (AABBLike r) -> Rectangle
toCanvasRect { position, size } =
    { x: x position
    , y: y position
    , width: x size
    , height: y size
    }