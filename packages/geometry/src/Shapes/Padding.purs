module Geometry.Shapes.Padding 
    ( Padding
    , PaddingPlacement(..)
    , aabbPadding
    , equalPadding
    , xyPadding
    , fourWayPadding
    ) where

import Loglude

import Data.Vec as Vec
import Geometry.Base (type (<+>), Attributes, FullGeometryConstructor, GenericGeometryAttributes, Geometry, GeometryAttributes, AllAttributes, bounds, group, rect, translate)
import Geometry.Vector (Vec2)
import Record as Record
import Record.Unsafe.Union (unsafeUnion)

type Padding = Vec D4 Number

-- | Fixed child keeps the corner of the child where it was before
-- | Fixed corner places the corner of the padding where the corner of the child was before.
data PaddingPlacement = FixedChild | FixedCorner

type PaddingAttributes :: Attributes
type PaddingAttributes r a = ( target :: Geometry a, amount :: Padding | r )

type OptionalPaddingAttributes :: Attributes
type OptionalPaddingAttributes r a = ( paddingPlacement :: PaddingPlacement | r )

equalPadding :: Number -> Padding
equalPadding = Vec.replicate d4

xyPadding :: Vec2 -> Padding
xyPadding a = a `Vec.concat` a

fourWayPadding :: Number -> Number -> Number -> Number -> Padding
fourWayPadding a b c d = Vec.cons a $ Vec.cons b $ Vec.cons c $ Vec.cons d Vec.empty

defaults :: forall a. AllAttributes OptionalPaddingAttributes a
defaults = { paddingPlacement: FixedCorner }

_aabbPadding :: forall a. Record ((PaddingAttributes <+> OptionalPaddingAttributes) (GenericGeometryAttributes Opt a) a) -> Geometry a
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
    process = case attributes.paddingPlacement of
        FixedCorner -> translate amountTopLeft
        FixedChild -> identity

aabbPadding :: forall a. FullGeometryConstructor OptionalPaddingAttributes PaddingAttributes a
aabbPadding attribs = unsafeUnion attribs defaults # _aabbPadding
