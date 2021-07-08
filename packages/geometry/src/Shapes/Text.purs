module Geometry.Shapes.Text 
    ( CustomTextAttributes(..)
    , text ) where

import Loglude

import Geometry.Base (type (<+>), AllAttributes, FullGeometryConstructor, Geometry, OptionalTextAttributes, TextAttributes)
import Geometry.Hiccup (class Hiccup, class IsAABB, buildGeometryBlueprint, noLocalCoordinates, pointInsideAABB, toAABB, translateByLens)
import Graphics.Canvas (Context2D)
import Loglude.UntypedArray (UntypedArray)

newtype CustomTextAttributes a = CustomTextAttributes
    (AllAttributes (TextAttributes <+> OptionalTextAttributes) a)

_text :: forall a. Ask Context2D => CustomTextAttributes a -> Geometry a
_text = buildGeometryBlueprint "Text"

text :: forall a. Ask Context2D => FullGeometryConstructor OptionalTextAttributes TextAttributes a
text = unsafeCoerce _text

---------- Typeclass instances
instance Ask Context2D => Hiccup CustomTextAttributes where
    translate = translateByLens (_Newtype <<< prop (Proxy :: _ "position"))
    pointInside = pointInsideAABB
    toHiccup = textToHiccup
    bounds = toAABB
    toLocalCoordinates = noLocalCoordinates
    children _ = []

instance Ask Context2D => IsAABB CustomTextAttributes where
    toAABB (CustomTextAttributes this) = do
        let metrics = measureText ask this.font this.text 
        { position: this.position
        , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
        }

derive instance Newtype (CustomTextAttributes a) _

---------- Foreign imports
type TextMetrics = 
    { width :: Number
    , fontBoundingBoxAscent :: Number
    }

foreign import textToHiccup :: forall a. CustomTextAttributes a -> UntypedArray
foreign import measureText :: Context2D -> String -> String -> TextMetrics