module Geometry.Shapes.Effectful 
    ( Effectful(..)
    , effectful
    , withResult
    , withReactiveRef
    , reffed ) where

import Loglude

import Effect.Ref as Ref
import Geometry.Base (Geometry)
import Geometry.Hiccup (class Hiccup, bounds, buildGeometryBlueprint, pointInside, toHiccup, toLocalCoordinates, translate)
import Loglude.ReactiveRef as RR

---------- Types
newtype Effectful a = Effectful (Effect (Geometry a))

---------- Helpers
-- | Embed an effect inside a geometry
effectful :: forall a. Effect (Geometry a) -> Geometry a
effectful = Effectful >>> buildGeometryBlueprint "Effectful"

-- | Express a geomery in terms of an effectful operation
withResult :: forall input a. Effect input -> (input -> Geometry a) -> Geometry a
withResult effect build = effectful $ effect <#> build

withReactiveRef :: forall payload r a. ReactiveRef r payload -> (payload -> Geometry a) -> Geometry a
withReactiveRef ref build = effectful $ RR.read ref <#> build

-- | Express a geomery as a function of some mutable value
reffed :: forall payload a. Ref payload -> (payload -> Geometry a) -> Geometry a
reffed ref build = effectful $ Ref.read ref <#> build

---------- Typeclass instances
derive instance Newtype (Effectful a) _

instance Hiccup Effectful where
    translate amount = over _Newtype $ map $ translate amount
    children = unwrap >>> map pure
    toHiccup = unwrap >=> toHiccup
    pointInside point = unwrap >=> pointInside point
    bounds = unwrap >=> bounds
    toLocalCoordinates shape point = do
        currentShape <- unwrap shape
        toLocalCoordinates currentShape point