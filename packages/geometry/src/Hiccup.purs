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

import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Geometry.Base (AABB, Geometry, rect)
import Geometry.Vector (Vec2)
import Loglude.UntypedArray (UntypedArray)

type HiccupClass = (Type -> Type) -> Constraint

class Hiccup :: HiccupClass 
class Hiccup f where
    translate :: forall a. Vec2 -> f a -> f a
    toHiccup :: forall a. f a -> Effect UntypedArray
    pointInside :: forall a. Vec2 -> f a -> Effect Boolean
    bounds :: forall a. f a -> Effect AABB
    children :: forall a. f a -> Effect (Array (Geometry a))
    toLocalCoordinates :: forall a. f a -> Vec2 -> Effect Vec2

class IsAABB :: HiccupClass
class IsAABB f where
    toAABB :: forall a. f a -> AABB

class GeometryWrapper :: HiccupClass
class GeometryWrapper f where
    unwrapGeometry :: forall a. f a -> Effect (Geometry a)

---------- Helpers
pointInsideAABB :: forall a f. IsAABB f => Vec2 -> f a -> Effect Boolean
pointInsideAABB point = toAABB >>> rect >>> pointInside point

translateByLens :: forall t. Setter' t Vec2 -> Vec2 -> t -> t
translateByLens lens = (+) >>> over lens

noLocalCoordinates :: forall f (a :: Type). f a -> Vec2 -> Effect Vec2
noLocalCoordinates = const pure

pointInsideLocalCoordinates :: forall f a. Hiccup f => GeometryWrapper f => Vec2 -> f a -> Effect Boolean
pointInsideLocalCoordinates point geometry = join (pointInside <$> projected <*> unwrapGeometry geometry)
    where
    projected = toLocalCoordinates geometry point

---------- Typeclass isntances
instance Hiccup Geometry where
    toHiccup = runEffectFn1 toHiccupGeometry
    pointInside = runEffectFn2 pointInsideGeometry
    bounds = runEffectFn1 boundsGeometry
    translate = translateGeometry
    children = runEffectFn1 childrenGeometry
    toLocalCoordinates = runEffectFn2 toLocalCoordinatesGeometry

instance GeometryWrapper Geometry where
    unwrapGeometry = pure
    
---------- Foreign imports
newtype WithHiccupDict f a = WithHiccupDict (Hiccup f => a)

buildGeometryBlueprint :: forall f a. Hiccup f => String -> f a -> Geometry a
buildGeometryBlueprint name = go
    where
    WithHiccupDict go = buildGeometryBlueprintImpl name

foreign import buildGeometryBlueprintImpl :: 
    forall f a. String -> WithHiccupDict f (f a -> Geometry a)

foreign import pointInsideGeometry :: forall a. EffectFn2 Vec2 (Geometry a) Boolean
foreign import toHiccupGeometry :: forall a. EffectFn1 (Geometry a) UntypedArray
foreign import boundsGeometry :: forall a. EffectFn1 (Geometry a) AABB
foreign import childrenGeometry :: forall a. EffectFn1 (Geometry a) (Array (Geometry a))
foreign import toLocalCoordinatesGeometry :: forall a. EffectFn2 (Geometry a) Vec2 Vec2
foreign import translateGeometry :: forall a. Vec2 -> Geometry a -> Geometry a