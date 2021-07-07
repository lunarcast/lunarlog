-- | Definitions for the core of the geometry package
module Geometry.Base 
    ( Geometry
    , ClickCheck(..)
    , FullConstructor
    , GenericGeometryAttributes
    , GeometryAttributes
    , IncompleteGeometryAttributes
    , FullGeometryConstructor
    , GeometryConstructor
    , ForeignGeometryConstructor
    , AABBLike
    , AABB
    , Attributes
    , ComposeAttributes
    , RectAttributes
    , CircleAttributes
    , GroupAttributes
    , OptionalGroupAttributes
    , TextAttributes
    , OptionalTextAttributes
    , CanvasMouseEvent
    , AllAttributes
    , WithGeometryAttributes
    , WithIncompleteGeometryAttributes
    , GenericEventAttributes
    , group
    , rect
    , circle
    , rawText
    , distanceToShape
    , distanceToShapeSquared
    , render
    , attributes
    , allAttributes
    , children
    , closestPoint
    , transform
    , transformVertices
    , type (<+>)) where

import Loglude

import Geometry.TextBaseline (TextBaseline)
import Geometry.Transform (TransformMatrix)
import Geometry.Vector (Vec2, distance, distanceSquared)
import Graphics.Canvas (Context2D)
import Prim.Row (class Union)

data Geometry :: Type -> Type
data Geometry a

data ClickCheck
    = MouseInside
    | MouseCloserThan Number

type GenericEventAttributes :: (Type -> Type) -> Attributes
type GenericEventAttributes f r action =
    ( onClick :: f (CanvasMouseEvent -> action) 
    | r )

type GenericGeometryAttributes :: (Type -> Type) -> Type -> Row Type
type GenericGeometryAttributes f action =
    ( fill :: f String
    , stroke :: f String
    , weight :: f Number
    , alpha :: f Number
    -- Here for debugging only (rn)
    , label :: f String
    | GenericEventAttributes f () action )

type GeometryAttributes a = GenericGeometryAttributes Id a
type IncompleteGeometryAttributes a = GenericGeometryAttributes Opt a

-- | Generic constructor which depends on the action and has optional attributes
type FullConstructor :: forall k. (Type -> Type) -> (Row Type -> Type -> Row k) -> (Row k -> Type -> Row Type) -> Type -> Type
type FullConstructor f optional required action
    = forall given rest.
    Union given rest (optional (GeometryAttributes action) action) => 
    Record (required given action) -> f action

-- | Constructor for geometries with optional parameters
type FullGeometryConstructor :: forall k. (Row Type -> Type -> Row k) -> (Row k -> Type -> Row Type) -> Type -> Type
type FullGeometryConstructor optional required action = FullConstructor Geometry optional required action

-- | Geometry constructor with no optional parameters
type GeometryConstructor required action = 
    FullGeometryConstructor Const required action

type ForeignGeometryConstructor required action
    = Record (required (GeometryAttributes action) action) -> Geometry action

type AABBLike r = ( position :: Vec2, size :: Vec2 | r )
type AABB = Record (AABBLike ())

type Attributes = Row Type -> Type -> Row Type

type AllAttributes :: Attributes -> Type -> Type
type AllAttributes attrs a = Record (attrs () a)

type WithGeometryAttributes :: Attributes -> Type -> Type
type WithGeometryAttributes attrs a = Record (attrs (GeometryAttributes a) a)

type WithIncompleteGeometryAttributes :: Attributes -> Type -> Type
type WithIncompleteGeometryAttributes attrs a = Record (attrs (IncompleteGeometryAttributes a) a)

type ComposeAttributes :: Attributes -> Attributes -> Attributes
type ComposeAttributes n m r a = n (m r a) a

infixr 0 type ComposeAttributes as <+>

type RectAttributes :: Attributes
type RectAttributes r a = AABBLike r

type CircleAttributes :: Attributes
type CircleAttributes r a = ( position :: Vec2, radius :: Number | r )

type GroupAttributes :: Attributes
type GroupAttributes r a = ( children :: Array (Geometry a) | r )

type OptionalGroupAttributes :: Attributes
type OptionalGroupAttributes r a = ( transform :: TransformMatrix | r )

type TextAttributes :: Attributes
type TextAttributes r a = 
    ( position :: Vec2
    , text :: String
    | r )

type OptionalTextAttributes :: Attributes
type OptionalTextAttributes r a = 
    ( baseline :: TextBaseline
    , font :: String 
    | r )

type CanvasMouseEvent = { buttons :: Int, position :: Vec2 }

---------- Constructors
group :: forall a. FullGeometryConstructor OptionalGroupAttributes GroupAttributes a 
group = unsafeCoerce _group

rect :: forall a. GeometryConstructor RectAttributes a
rect = unsafeCoerce _rect

circle :: forall a. GeometryConstructor CircleAttributes a
circle = unsafeCoerce _circle

rawText :: forall a. FullGeometryConstructor OptionalTextAttributes TextAttributes a
rawText = unsafeCoerce _text

---------- Heleprs
distanceToShape :: forall a. Geometry a -> Vec2 -> Number
distanceToShape geometry point = distance point (closestPoint geometry point)

distanceToShapeSquared :: forall a. Geometry a -> Vec2 -> Number
distanceToShapeSquared geometry point = distanceSquared point (closestPoint geometry point)

allAttributes :: forall a. Geometry a -> Object Foreign
allAttributes = unsafeCoerce attributes

---------- Foreign imports
foreign import _rect :: forall a. ForeignGeometryConstructor RectAttributes a
foreign import _circle :: forall a. ForeignGeometryConstructor CircleAttributes a
foreign import _group :: forall a. ForeignGeometryConstructor 
    (OptionalGroupAttributes <+> GroupAttributes) a
foreign import _text :: forall a. ForeignGeometryConstructor 
    (OptionalTextAttributes <+> TextAttributes) a

foreign import render :: forall a. Context2D -> Geometry a -> Effect Unit
foreign import attributes :: forall a. Geometry a -> Record (IncompleteGeometryAttributes a)
foreign import children :: forall a. Geometry a -> Array (Geometry a)
foreign import closestPoint :: forall a. Geometry a -> Vec2 -> Vec2

foreign import transform :: forall a. TransformMatrix -> Geometry a -> Geometry a
foreign import transformVertices :: forall a. (Vec2 -> Vec2) -> Geometry a -> Geometry a
