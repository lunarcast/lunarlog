module Geometry.Hiccup (HiccupConfig, buildGeometryBlueprint) where

import Loglude

import Geometry.Base (AABBLike, Geometry)
import Loglude.UntypedArray (UntypedArray)

type HiccupConfig :: (Type -> Type) -> Type
type HiccupConfig s =
    { _type :: String
    , toHiccup :: forall a. s a -> UntypedArray 
    , aabbLike :: forall a. Opt (s a -> Effect (Record (AABBLike ())))
    }

foreign import buildGeometryBlueprint :: 
    forall f. HiccupConfig f -> forall a. f a -> Geometry a