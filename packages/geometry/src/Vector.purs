module Geometry.Vector where

import Loglude
import Data.Vec ((!!))

type Vec2 = Vec D2 Number

x :: Vec2 -> Number
x = (_ !! d0)

y :: Vec2 -> Number
y = (_ !! d1)

foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number