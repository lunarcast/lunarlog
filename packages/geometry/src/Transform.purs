module Geometry.Transform where

import Loglude

import Geometry.Vector (Vec2)

newtype TransformMatrix = TransformMatrix (Vec D6 Number)

---------- Foreign imports
foreign import translate :: Vec D2 Number -> TransformMatrix
foreign import rotate :: Number -> TransformMatrix
foreign import scale :: Number -> TransformMatrix
foreign import scaleXY :: Vec D2 Number -> TransformMatrix
foreign import inverse :: TransformMatrix -> TransformMatrix
foreign import transform :: Vec D2 Number -> Number -> Vec D2 Number -> TransformMatrix
foreign import multiplyVector :: TransformMatrix -> Vec2 -> Vec2
foreign import composeMatrices :: TransformMatrix -> TransformMatrix -> TransformMatrix
foreign import identityMatrix :: TransformMatrix
foreign import rotateAround :: Vec2 -> Number -> TransformMatrix
---------- Typeclass instances
instance Semigroup TransformMatrix where
    append = composeMatrices

instance Monoid TransformMatrix where
    mempty = identityMatrix
