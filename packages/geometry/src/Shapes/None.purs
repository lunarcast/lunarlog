module Geometry.Shapes.None (none) where

import Loglude

import Geometry.Vector (Vec2)
import Geometry.Base (Geometry)
import Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint)
import Loglude.UntypedArray as UntypedArray

newtype None :: Type -> Type
newtype None a = None { position :: Vec2 }

noneConfig :: HiccupConfig None
noneConfig = 
    { translate: (+) >>> over _position
    , toHiccup: const $ UntypedArray.nil
    , aabbLike: opt \(None { position }) -> pure { position, size: zero }
    }

none :: forall a. Vec2 -> Geometry a
none position = buildGeometryBlueprint noneConfig (None { position })

---------- Lenses
_position :: forall a. Lens' (None a) Vec2
_position = _Newtype <<< prop (Proxy :: _ "position")

--------- Typeclass instances
derive instance Newtype (None a) _