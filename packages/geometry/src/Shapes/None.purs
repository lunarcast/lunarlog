module Geometry.Shapes.None (none) where

import Loglude

import Geometry.Base (Geometry)
import Geometry.Hiccup (class Hiccup, class IsAABB, buildGeometryBlueprint, noLocalCoordinates, pointInsideAABB, toAABB, translateByLens)
import Geometry.Vector (Vec2)
import Loglude.UntypedArray as UntypedArray

newtype None :: Type -> Type
newtype None a = None { position :: Vec2 }

none :: forall a. Vec2 -> Geometry a
none position = buildGeometryBlueprint "None" (None { position })

--------- Typeclass instances
derive instance Newtype (None a) _

instance Hiccup None where
    translate = translateByLens _position
    toHiccup = const UntypedArray.nil
    bounds = toAABB
    pointInside = pointInsideAABB
    toLocalCoordinates = noLocalCoordinates
    children _ = []

instance IsAABB None where
    toAABB (None { position }) = { position, size: zero } 

---------- Lenses
_position :: forall a. Lens' (None a) Vec2
_position = _Newtype <<< prop (Proxy :: _ "position")