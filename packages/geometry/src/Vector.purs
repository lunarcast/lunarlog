module Geometry.Vector where

import Loglude
import Data.Vec ((!!))

type Vec2 = Vec D2 Number

x :: Vec2 -> Number
x = (_ !! d0)

y :: Vec2 -> Number
y = (_ !! d1)

toTuple :: forall a. Vec D2 a -> a /\ a
toTuple vec = (vec !! d0) /\ (vec !! d1) 

fromTuple :: forall a. a /\ a -> Vec D2 a
fromTuple = uncurry vec2

foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number