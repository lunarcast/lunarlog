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
    , children
    , toLocalCoordinates
    , noLocalCoordinates
    , pointInsideLocalCoordinates
    ) where

import Loglude

import Geometry.Base (AABB, Geometry, rect)
import Geometry.Vector (Vec2)
import Loglude.UntypedArray (UntypedArray)
import Prelude (identity)

type HiccupClass = (Type -> Type) -> Constraint

class Hiccup :: HiccupClass 
class Hiccup f where
    toHiccup :: forall a. f a -> UntypedArray
    translate :: forall a. Vec2 -> f a -> f a
    pointInside :: forall a. Vec2 -> f a -> Boolean
    bounds :: forall a. f a -> AABB
    children :: forall a. f a -> Array (Geometry a)
    toLocalCoordinates :: forall a. f a -> Vec2 -> Vec2

class IsAABB :: HiccupClass
class IsAABB f where
    toAABB :: forall a. f a -> AABB

class GeometryWrapper :: HiccupClass
class GeometryWrapper f where
    unwrapGeometry :: forall a. f a -> Geometry a

---------- Helpers
pointInsideAABB :: forall a f. IsAABB f => Vec2 -> f a -> Boolean
pointInsideAABB point = toAABB >>> rect >>> pointInsideGeometry point

translateByLens :: forall t. Setter' t Vec2 -> Vec2 -> t -> t
translateByLens lens = (+) >>> over lens

noLocalCoordinates :: forall f (a :: Type). f a -> Vec2 -> Vec2
noLocalCoordinates = const identity

pointInsideLocalCoordinates :: forall f a. Hiccup f => GeometryWrapper f => Vec2 -> f a -> Boolean
pointInsideLocalCoordinates point geometry = pointInside projected $ unwrapGeometry geometry
    where
    projected = toLocalCoordinates geometry point

---------- Typeclass isntances
instance Hiccup Geometry where
    toHiccup = toHiccupGeometry
    pointInside = pointInsideGeometry
    bounds = boundsGeometry
    translate = translateGeometry
    children = childrenGeometry
    toLocalCoordinates = toLocalCoordinatesGeometry

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
foreign import childrenGeometry :: forall a. Geometry a -> Array (Geometry a)
foreign import toLocalCoordinatesGeometry :: forall a. Geometry a -> Vec2 -> Vec2