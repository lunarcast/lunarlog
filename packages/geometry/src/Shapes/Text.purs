module Geometry.Shapes.Text 
    ( CustomTextAttributes(..)
    , text ) where

import Loglude

import Data.Lens (Lens', over)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Geometry.Base (type (<+>), FullGeometryConstructor, Geometry, OptionalTextAttributes, TextAttributes)
import Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint)
import Geometry.Vector (Vec2)
import Graphics.Canvas (Context2D, measureText, restore, save, setFont)
import Loglude.UntypedArray (UntypedArray)

newtype CustomTextAttributes a = CustomTextAttributes
    (Record ((TextAttributes <+> OptionalTextAttributes) () a))

textConfig :: Ask Context2D => HiccupConfig CustomTextAttributes
textConfig =
    { toHiccup: textToHiccup
    , aabbLike: opt aabb
    , translate:  (+) >>> over _position
    }
    where
    aabb (CustomTextAttributes this) = ado
        save ask
        setFont ask this.font
        metrics <- unsafeCoerce <$> measureText ask this.text 
        restore ask
        in { position: this.position
           , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
           }

_text :: forall a. Ask Context2D => CustomTextAttributes a -> Geometry a
_text = buildGeometryBlueprint textConfig

text :: forall a. Ask Context2D => FullGeometryConstructor OptionalTextAttributes TextAttributes a
text = unsafeCoerce _text

foreign import textToHiccup :: forall a. CustomTextAttributes a -> UntypedArray

---------- Typeclass instances
derive instance Newtype (CustomTextAttributes a) _

---------- Lenses
_position :: forall a. Lens' (CustomTextAttributes a) Vec2
_position = _Newtype <<< prop (Proxy :: _ "position")