module Geometry.Shapes.Text 
    ( CustomTextAttributes
    , text ) where

import Loglude

import Data.Undefined.NoProblem (opt)
import Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint)
import Geometry.Base (type (<+>), FullGeometryConstructor, Geometry, OptionalTextAttributes, TextAttributes)
import Graphics.Canvas (Context2D, measureText)
import Loglude.Ask (class Ask, ask)
import Loglude.UntypedArray (UntypedArray)

newtype CustomTextAttributes a = CustomTextAttributes
    (Record ((TextAttributes <+> OptionalTextAttributes) () a))

textConfig :: Ask Context2D => HiccupConfig CustomTextAttributes
textConfig =
    { _type: "aabb"
    , toHiccup: textToHiccup
    , aabbLike: opt aabb
    }
    where
    aabb (CustomTextAttributes this) = ado
        metrics <- unsafeCoerce <$> measureText ask this.text 
        in { position: this.position
           , size: vec2 metrics.width (metrics.fontBoundingBoxDescent + metrics.fontBoundingBoxAscent)
           }

_text :: forall a. Ask Context2D => CustomTextAttributes a -> Geometry a
_text = buildGeometryBlueprint textConfig

text :: forall a. Ask Context2D => FullGeometryConstructor OptionalTextAttributes TextAttributes a
text = unsafeCoerce _text

foreign import textToHiccup :: forall a. CustomTextAttributes a -> UntypedArray