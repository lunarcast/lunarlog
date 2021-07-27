module Geometry.Vector 
    ( Vec2
    , Axis(..)
    , x
    , y
    , toTuple
    , fromTuple
    , distance
    , distanceSquared
    , other
    , indexByAxis
    , mapAxis
    , lmapAxis
    , rmapAxis
    , bimapAxis
    , buildFromAxis
    , greaterThan
    , smallerThan
    , multiplyScalar
    , dotProduct
    , _insideVector
    , _x
    , _y
    , _axis
    , _otherAxis
    ) where

import Loglude

import Data.Typelevel.Num (class Lt, class Nat)
import Data.Vec ((!!))
import Data.Vec as Vec

type Vec2 = Vec D2 Number

x :: Vec2 -> Number
x = (_ !! d0)

y :: Vec2 -> Number
y = (_ !! d1)

toTuple :: forall a. Vec D2 a -> a /\ a
toTuple vec = (vec !! d0) /\ (vec !! d1) 

fromTuple :: forall a. a /\ a -> Vec D2 a
fromTuple = uncurry vec2

smallerThan :: Vec2 -> Vec2 -> Boolean
smallerThan a b = x a < x b && y a < y b

greaterThan :: Vec2 -> Vec2 -> Boolean
greaterThan a b = x a > x b && y a > y b

---------- Stuff related to axis
data Axis = X | Y

other :: Axis -> Axis
other X = Y
other Y = X

indexByAxis :: Axis -> Vec2 -> Number
indexByAxis X = x
indexByAxis Y = y

buildFromAxis :: Axis -> Number -> Number -> Vec2
buildFromAxis X a b = vec2 a b 
buildFromAxis Y a b = vec2 b a

mapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
mapAxis axis = over (_axis axis)

lmapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
lmapAxis = mapAxis

rmapAxis :: Axis -> (Number -> Number) -> Vec2 -> Vec2
rmapAxis = other >>> mapAxis

bimapAxis :: Axis -> (Number -> Number) -> (Number -> Number) -> Vec2 -> Vec2
bimapAxis axis f g = over (_axis axis) f >>> over (_otherAxis axis) g

---------- Lenses
_insideVector :: forall a s i. Nat i => Lt i s => i -> Lens' (Vec s a) a
_insideVector index = lens get set
    where 
    get vec = vec !! index
    set vec newX = Vec.updateAt index newX vec

_x :: Lens' Vec2 Number
_x = _insideVector d0

_y :: Lens' Vec2 Number
_y = _insideVector d1

_axis :: Axis -> Lens' Vec2 Number
_axis X = _x
_axis Y = _y

_otherAxis :: Axis -> Lens' Vec2 Number
_otherAxis axis = _axis (other axis)

---------- Foreign stuff
foreign import distance :: Vec2 -> Vec2 -> Number
foreign import distanceSquared :: Vec2 -> Vec2 -> Number
foreign import dotProduct :: Vec2 -> Vec2 -> Number
foreign import multiplyScalar :: Vec2 -> Number -> Vec2