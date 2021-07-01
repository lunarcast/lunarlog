module Geometry.Shapes.Padding 
    ( Padding
    , PaddingPlacement(..)
    , aabbPadding
    , equalPadding
    , xyPadding
    ) where

import Loglude

import Data.Vec as Vec
import Geometry.Base (type (<+>), Attributes, GenericGeometryAttributes, Geometry, GeometryAttributes, FullGeometryConstructor, bounds, group, rect, translate)
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
_aabbPadding attributes = process $ group
    { children: 
        [ rect $ Record.union (unsafeCoerce attributes :: Record (GeometryAttributes a))
            { position: position - amountTopLeft
            , size: size + amountTopLeft + amountBottomRight
            } 
        , attributes.target
        ]
    }
    where
    { position, size } = bounds attributes.target
    amountTopLeft = Vec.take d2 attributes.amount
    amountBottomRight = Vec.drop d2 attributes.amount

    -- | The amount we shift all the children by
    process = case fromOpt FixedCorner attributes.paddingPlacement of
        FixedCorner -> translate amountTopLeft
        FixedChild -> identity

aabbPadding :: forall a. FullGeometryConstructor (OptionalPaddingAttributes Id) PaddingAttributes a
aabbPadding = unsafeCoerce _aabbPadding

