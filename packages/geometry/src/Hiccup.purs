module Geometry.Hiccup 
    ( class Hiccup 
    , class IsAABB
    , class GeometryWrapper
    , toHiccup
    , translate
    , bounds
    , pointInside
    , toAABB
    , unwrapGeometry
    , pointInsideAABB
    , buildGeometryBlueprint
    , translateByLens
    ) where

import Loglude

import Geometry.Base (AABB, Geometry, rect)
import Geometry.Vector (Vec2)
import Loglude.UntypedArray (UntypedArray)

type HiccupClass = (Type -> Type) -> Constraint

class Hiccup :: HiccupClass 
class Hiccup f where
    toHiccup :: forall a. f a -> UntypedArray
    translate :: forall a. Vec2 -> f a -> f a
    pointInside :: forall a. Vec2 -> f a -> Boolean
    bounds :: forall a. f a -> AABB

class IsAABB :: HiccupClass
class IsAABB f where
    toAABB :: forall a. f a -> AABB

class GeometryWrapper :: HiccupClass
class GeometryWrapper f where
    unwrapGeometry :: forall a. f a -> Geometry a

pointInsideAABB :: forall a f. IsAABB f => Vec2 -> f a -> Boolean
pointInsideAABB point = toAABB >>> rect >>> pointInsideGeometry point

translateByLens :: forall t. Setter' t Vec2 -> Vec2 -> t -> t
translateByLens lens = (+) >>> over lens

---------- Typeclass isntances
instance Hiccup Geometry where
    toHiccup = toHiccupGeometry
    pointInside = pointInsideGeometry
    bounds = boundsGeometry
    translate = translateGeometry

instance GeometryWrapper Geometry where
    unwrapGeometry = identity
    
---------- Foreign imports
newtype WithHiccupDict f a = WithHiccupDict (Hiccup f => a)

buildGeometryBlueprint :: forall f a. Hiccup f => String -> f a -> Geometry a
buildGeometryBlueprint name = go
    where
    WithHiccupDict go = buildGeometryBlueprintImpl name

foreign import buildGeometryBlueprintImpl :: 
    forall f a. String -> WithHiccupDict f (f a -> Geometry a)

foreign import pointInsideGeometry :: forall a. Vec2 -> Geometry a -> Boolean
foreign import toHiccupGeometry :: forall a. Geometry a -> UntypedArray
foreign import boundsGeometry :: forall a. Geometry a -> AABB
foreign import translateGeometry :: forall a. Vec2 -> Geometry a -> Geometry a