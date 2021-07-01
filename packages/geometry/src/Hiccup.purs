module Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint) where

import Loglude

import Geometry.Vector (Vec2)
import Geometry.Base (AABBLike, Geometry)
import Loglude.UntypedArray (UntypedArray)

type HiccupConfig :: (Type -> Type) -> Type
type HiccupConfig s =
    { toHiccup :: forall a. s a -> UntypedArray 
    , aabbLike :: forall a. Opt (s a -> Effect (Record (AABBLike ())))
    , translate :: forall a. Vec2 -> s a -> s a
    }

foreign import buildGeometryBlueprint :: 
    forall f. HiccupConfig f -> forall a. f a -> Geometry a