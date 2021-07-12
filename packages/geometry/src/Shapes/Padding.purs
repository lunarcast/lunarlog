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
type PaddingAttributes a r = 
    ( target :: Geometry a
    , amount :: Padding
    , paddingPlacement :: Opt PaddingPlacement
    , paddingModifiers :: Opt (Record (GeometryAttributes a ()))
    | r )

---------- Constructors
equalPadding :: Number -> Padding
equalPadding = Vec.replicate d4

xyPadding :: Vec2 -> Padding
xyPadding a = a `Vec.concat` a

fourWayPadding :: Number -> Number -> Number -> Number -> Padding
fourWayPadding a b c d = Vec.cons a $ Vec.cons b $ Vec.cons c $ Vec.cons d Vec.empty

aabbPadding :: 
    forall given action. 
    Ask Context2D => 
    Closed.Coerce given (Record (PaddingAttributes action ())) => 
    given -> 
    Geometry action
aabbPadding = Closed.coerce >>>  _aabbPadding

---------- Implementation
_aabbPadding :: 
    forall action. 
    Ask Context2D =>
    Record (PaddingAttributes action ()) -> 
    Geometry action
_aabbPadding attributes =
    process $ group
        { children: 
            [ case bounds attributes.target of
                Just { position, size } -> rect $ Record.merge (Opt.fromOpt (Closed.coerce {}) attributes.paddingModifiers)
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