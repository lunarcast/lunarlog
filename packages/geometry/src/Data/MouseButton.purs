module Data.MouseButton (MouseButtons(..), MouseButton, isPressed, nothingPressed, leftButton, rightButton) where

import Loglude
import Data.Int.Bits (and)


newtype MouseButtons = MouseButtons Int
type MouseButton = MouseButtons

nothingPressed :: MouseButtons -> Boolean
nothingPressed (MouseButtons bits) = bits == 0

isPressed :: MouseButtons -> MouseButton -> Boolean
isPressed (MouseButtons bits) (MouseButtons button) = 0 /= (bits `and` button)

leftButton :: MouseButton
leftButton = MouseButtons 1 

rightButton :: MouseButton
rightButton = MouseButtons 2 