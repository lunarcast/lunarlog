module Geometry.Shapes.Transformed (Transformed(..), transformed, visuallyTransformed, transformed_) where

import Loglude

import Debug (spy)
import Geometry.Base (Geometry)
import Geometry.Base as Geometry
import Geometry.Hiccup (class GeometryWrapper, class Hiccup, bounds, buildGeometryBlueprint, pointInside, toHiccup)
import Geometry.Hiccup as Hiccup
import Geometry.Transform (TransformMatrix, multiplyVector, translate)

-- | Attributes for the transformed 
newtype Transformed a = Transformed
    { transform :: TransformMatrix
    , target :: Geometry a
    , transformBounds :: Boolean
    }
    
transformed_ :: Transformed ~> Geometry
transformed_ = buildGeometryBlueprint "Transformed"

transformed :: TransformMatrix -> Geometry ~> Geometry
transformed transform target = transformed_ $ Transformed { transform, target, transformBounds: true }

-- | Does not affect the bounds of the element. Useful for offseting something inside padding.
visuallyTransformed :: TransformMatrix -> Geometry ~> Geometry
visuallyTransformed transform target = transformed_ $ Transformed { transform, target, transformBounds: false }

---------- Typeclass instances
instance Hiccup Transformed where
    toHiccup (Transformed { transform, target }) = toHiccup $ Geometry.group { transform, children: [target] }
    pointInside point (Transformed { transform, target }) = pointInside (multiplyVector transform point) target
    translate amount shape 
        | view _transformBounds shape = over _transform (_ <> translate amount) shape
        | otherwise = over _target (Hiccup.translate amount) shape
    bounds (Transformed { transform, target, transformBounds })
        | transformBounds = bounds $ Geometry.transform transform $ Geometry.rect $ bounds target
        | otherwise = bounds target

instance GeometryWrapper Transformed where
    unwrapGeometry (Transformed { transform, target }) = Geometry.transform transform target

derive instance Newtype (Transformed a) _

---------- Lenses
_transform :: forall a. Lens' (Transformed a) TransformMatrix
_transform = _Newtype <<< prop (Proxy :: _ "transform")

_transformBounds :: forall a. Lens' (Transformed a) Boolean
_transformBounds = _Newtype <<< prop (Proxy :: _ "transformBounds")

_target :: forall a. Lens' (Transformed a) (Geometry a)
_target = _Newtype <<< prop (Proxy :: _ "target")