module Geometry.Shapes.Transformed 
    ( Transformed(..)
    , TransformedAttributes
    , OptionalTransformedAttributes
    , transformed
    ) where

import Loglude

import Geometry.Base (type (<+>), AllAttributes, Attributes, FullGeometryConstructor, GenericEventAttributes, Geometry)
import Geometry.Base as Geometry
import Geometry.Hiccup (class GeometryWrapper, class Hiccup, bounds, buildGeometryBlueprint, pointInside, toHiccup, toLocalCoordinates)
import Geometry.Hiccup as Hiccup
import Geometry.Transform (TransformMatrix, inverse, multiplyVector, translate)
import Record.Unsafe.Union (unsafeUnion)

type TransformedAttributes :: Attributes
type TransformedAttributes r a =
    ( transform :: TransformMatrix
    , target :: Geometry a
    | r )

type OptionalTransformedAttributes :: Attributes
type OptionalTransformedAttributes r a =
    ( transformBounds :: Boolean | r )

-- | Attributes for the transformed 
newtype Transformed a = Transformed 
    (AllAttributes (TransformedAttributes <+> OptionalTransformedAttributes <+> GenericEventAttributes Opt) a)
    
transformed_ :: Transformed ~> Geometry
transformed_ = buildGeometryBlueprint "Transformed"

defaults :: forall a. AllAttributes OptionalTransformedAttributes a
defaults = 
    { transformBounds: true }

transformed :: forall a. FullGeometryConstructor OptionalTransformedAttributes TransformedAttributes a
transformed = unsafeCoerce >>> flip unsafeUnion defaults >>> unsafeCoerce transformed_ 

---------- Typeclass instances
instance Hiccup Transformed where
    pointInside point geometry@(Transformed { target }) = pointInside projected target
        where
        projected = toLocalCoordinates geometry point

    toHiccup (Transformed { transform, target }) = toHiccup $ Geometry.group { transform, children: [target] }
    children = view _target >>> pure
    translate amount shape 
        | view _transformBounds shape = over _transform (_ <> translate amount) shape
        | otherwise = over _target (Hiccup.translate amount) shape
    bounds (Transformed { transform, target, transformBounds })
        | transformBounds = bounds $ Geometry.transform transform $ Geometry.rect $ bounds target
        | otherwise = bounds target

    -- TODO: find a way to cache the inverse
    toLocalCoordinates = view _transform >>> inverse >>> multiplyVector

instance GeometryWrapper Transformed where
    unwrapGeometry = view _target

derive instance Newtype (Transformed a) _

---------- Lenses
_transform :: forall a. Lens' (Transformed a) TransformMatrix
_transform = _Newtype <<< prop (Proxy :: _ "transform")

_transformBounds :: forall a. Lens' (Transformed a) Boolean
_transformBounds = _Newtype <<< prop (Proxy :: _ "transformBounds")

_target :: forall a. Lens' (Transformed a) (Geometry a)
_target = _Newtype <<< prop (Proxy :: _ "target")