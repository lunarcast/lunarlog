module Geometry.TextBaseline where

import Loglude

data TextBaseline

top :: TextBaseline
top = unsafeCoerce "top"

hanging :: TextBaseline
hanging = unsafeCoerce "hanging"

middle :: TextBaseline
middle = unsafeCoerce "middle"

alphabetic :: TextBaseline
alphabetic = unsafeCoerce "alphabetic"

ideographic :: TextBaseline
ideographic = unsafeCoerce "ideographic"

bottom :: TextBaseline
bottom = unsafeCoerce "bottom"
