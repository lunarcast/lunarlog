module Geometry.Shapes.Padding 
    ( Padding
    , PaddingPlacement
    , aabbPadding
    , equalPadding
    , xyPadding
    ) where

import Loglude (D4, Opt, Vec, d2, d4, unsafeCoerce)
import Prelude

import Data.Undefined.NoProblem (fromOpt)
import Data.Vec as Vec
import Geometry.Transform (translate)
import Geometry.Types (type (<+>), Attributes, GenericGeometryAttributes, Geometry, GeometryAttributes, Id, FullGeometryConstructor, bounds, group, rect)
import Geometry.Vector (Vec2)
import Record as Record

type Padding = Vec D4 Number

-- | Fixed child keeps the corner of the child where it was before
-- | Fixed corner places the corner of the padding where the corner of the child was before.
data PaddingPlacement = FixedChild | FixedCorner

type PaddingAttributes :: Attributes
type PaddingAttributes r a = ( target :: Geometry a, amount :: Padding | r )

type OptionalPaddingAttributes :: (Type -> Type) -> Attributes
type OptionalPaddingAttributes f r a = ( paddingPlacement :: f PaddingPlacement | r )

equalPadding :: Number -> Padding
equalPadding = Vec.replicate d4

xyPadding :: Vec2 -> Padding
xyPadding a = a `Vec.concat` a

_aabbPadding :: forall a. Record ((PaddingAttributes <+> OptionalPaddingAttributes Opt) (GenericGeometryAttributes Opt a) a) -> Geometry a
_aabbPadding attributes = group
    { children: 
        [ rect $ Record.union (unsafeCoerce attributes :: Record (GeometryAttributes a))
            { position: position - amountTopLeft
            , size: size + amountTopLeft + amountBottomRight
            } 
        , attributes.target
        ]
    , transform: case fromOpt FixedCorner attributes.paddingPlacement of
        FixedCorner -> translate amountTopLeft
        FixedChild -> mempty
    }
    where
    { position, size } = bounds attributes.target
    amountTopLeft = Vec.take d2 attributes.amount
    amountBottomRight = Vec.drop d2 attributes.amount

aabbPadding :: forall a. FullGeometryConstructor (OptionalPaddingAttributes Id) PaddingAttributes a
aabbPadding = unsafeCoerce _aabbPadding

