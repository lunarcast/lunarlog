module Geometry.Hiccup where

import Loglude

import Geometry.Types (AABBLike, Geometry)
import Loglude.UntypedArray (UntypedArray)

type AABBDict = Record (AABBLike ()) 

type HiccupConfig :: (Type -> Type) -> Type
type HiccupConfig s =
    { _type :: String
    , toHiccup :: forall a. s a -> UntypedArray 
    , aabbLike :: forall a. Opt (s a -> Effect AABBDict)
    }

foreign import buildGeometryBlueprint :: 
    forall f. HiccupConfig f -> forall a. f a -> Geometry a