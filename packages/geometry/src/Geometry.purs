module Geometry 
    ( module Geometry.Base
    , module Geometry.Hiccup
    , module Geometry.Tea
    , module Geometry.TextBaseline
    , module Geometry.Transform
    , module Geometry.Vector
    , module Geometry.Shapes.Text
    , module Geometry.Shapes.Padding
    , module Graphics.Canvas
    ) where

import Geometry.Base
import Geometry.Vector 
import Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint)
import Geometry.Tea (SetupArgs, Tea, launchTea)
import Geometry.TextBaseline (TextBaseline)
import Geometry.Shapes.Padding (Padding, PaddingPlacement, aabbPadding, equalPadding, xyPadding)
import Geometry.Shapes.Text (CustomTextAttributes, text)
import Geometry.Transform (TransformMatrix(..))


import Graphics.Canvas (Context2D)