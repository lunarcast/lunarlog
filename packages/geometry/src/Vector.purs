module Geometry.Vector where

import Loglude

type Vec2 = Vec D2 Int

foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number