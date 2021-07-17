module Geometry.Shapes.Padding 
    ( Padding
    , PaddingPlacement(..)
    , aabbPadding
    , equalPadding
    , xyPadding
    , fourWayPadding
    ) where

import Loglude

import Data.Undefined.NoProblem.Closed as Closed
import Data.Vec as Vec
import Geometry.Base (Attributes, Geometry(..), GeometryAttributes, bounds, group, rect, translate)
import Geometry.Vector (Vec2)
import Graphics.Canvas (Context2D)
import Loglude as Opt
import Record as Record

---------- Types
type Padding = Vec D4 Number

-- | Fixed child keeps the corner of the child where it was before
-- | Fixed corner places the corner of the padding where the corner of the child was before.
data PaddingPlacement = FixedChild | FixedCorner

type PaddingAttributes :: Attributes
type PaddingAttributes id action r = 
    ( target :: Geometry id action
    , amount :: Padding
    , paddingPlacement :: Opt PaddingPlacement
    -- | Attributes applied to the rect acting as the padding
    , paddingModifiers :: Opt (Record (GeometryAttributes id action ()))
    -- | Attributes applied to the group containing the padding and the target
    , parentModifiers :: Opt (Record (GeometryAttributes id action ()))
    | r )

---------- Constructors
equalPadding :: Number -> Padding
equalPadding = Vec.replicate d4

xyPadding :: Vec2 -> Padding
xyPadding a = a `Vec.concat` a

fourWayPadding :: Number -> Number -> Number -> Number -> Padding
fourWayPadding a b c d = Vec.cons a $ Vec.cons b $ Vec.cons c $ Vec.cons d Vec.empty

aabbPadding :: 
    forall given id action.
    Ask Context2D => 
    Closed.Coerce given (Record (PaddingAttributes id action ())) => 
    given -> 
    Geometry id action
aabbPadding = Closed.coerce >>>  _aabbPadding

---------- Implementation
_aabbPadding :: 
    forall id action. 
    Ask Context2D =>
    Record (PaddingAttributes id action ()) -> 
    Geometry id action
_aabbPadding attributes =
    process $ group $ Record.merge parentModifiers 
        { children: 
            [ case bounds attributes.target of
                Just { position, size } -> rect $ Record.merge paddingModifiers 
                    { position: position - amountTopLeft
                    , size: size + amountTopLeft + amountBottomRight
                    , label: "Padding"
                    } 
                Nothing -> None zero
            , attributes.target
            ]
        , label: "Padding container"
        }
    where
    -- | The amount we shift all the children by
    process = case fromOpt FixedCorner attributes.paddingPlacement of
        FixedCorner -> translate amountTopLeft
        FixedChild -> identity

    amountTopLeft = Vec.take d2 attributes.amount
    amountBottomRight = Vec.drop d2 attributes.amount

    parentModifiers = Opt.fromOpt (Closed.coerce {}) attributes.parentModifiers
    paddingModifiers = Opt.fromOpt (Closed.coerce {}) attributes.paddingModifiers