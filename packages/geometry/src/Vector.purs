module Geometry.Vector where

import Loglude
import Data.Vec ((!!))

type Vec2 = Vec D2 Int

x :: Vec2 -> Int
x = (_ !! d0)

y :: Vec2 -> Int
y = (_ !! d1)

foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number