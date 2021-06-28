module Geometry.Types where

import Loglude

import Geometry.TextBaseline (TextBaseline)
import Geometry.Vector (Vec2, distance, distanceSquared)
import Graphics.Canvas (Context2D)
import Prim.Row (class Union)

data Geometry :: Type -> Type
data Geometry a

type Id :: forall k. k -> k
type Id a = a

type Const :: forall k1 k2. k1 -> k2 -> k1
type Const a b = a

data ClickCheck
    = MouseInside
    | MouseCloserThan Number

type GenericGeometryAttributes :: forall k. (Type -> k) -> Type -> Row k
type GenericGeometryAttributes f action = 
    ( fill :: f String
    , stroke :: f String
    , onClick :: f (CanvasMouseEvent -> action) 
    , clickChecker :: f ClickCheck
    )

type GeometryAttributes a = GenericGeometryAttributes Id a
type IncompleteGeometryAttributes a = GenericGeometryAttributes Opt a

type FullGeometryConstructor :: forall k. (Row Type -> Type -> Row k) -> (Row k -> Type -> Row Type) -> Type -> Type
type FullGeometryConstructor optional required action
    = forall given rest.
    Union given rest (optional (GeometryAttributes action) action) => 
    Record (required given action) -> Geometry action

-- | Geometry constructor with no optional parameters
type GeometryConstructor required action = 
    FullGeometryConstructor Const required action

type ForeignGeometryConstructor required action
    = Record (required (GeometryAttributes action) action) -> Geometry action

type AABBLike r = ( position :: Vec2, size :: Vec2 | r )

type Attributes = Row Type -> Type -> Row Type

type ComposeAttributes :: Attributes -> Attributes -> Attributes
type ComposeAttributes n m r a = n (m r a) a

infixr 0 type ComposeAttributes as <+>

type RectAttributes :: Attributes
type RectAttributes r a = AABBLike r

type CircleAttributes :: Attributes
type CircleAttributes r a = ( position :: Vec2, radius :: Int | r )

type GroupAttributes :: Attributes
type GroupAttributes r a = ( children :: Array (Geometry a) | r )

type TextAttributes :: Attributes
type TextAttributes r a = ( position :: Vec2, text :: String | r )

type OptionalTextAttributes :: Attributes
type OptionalTextAttributes r a = ( baseline :: TextBaseline | r )

type CanvasMouseEvent = { buttons :: Int, position :: Vec2 }

---------- Constructors
group :: forall a. GeometryConstructor GroupAttributes a 
group = unsafeCoerce _group

rect :: forall a. GeometryConstructor RectAttributes a
rect = unsafeCoerce _rect

circle :: forall a. GeometryConstructor CircleAttributes a
circle = unsafeCoerce _circle

rawText :: forall a. FullGeometryConstructor OptionalTextAttributes TextAttributes a
rawText = unsafeCoerce _text

none :: forall a. Geometry a 
none = group { children: [] }

---------- Heleprs
isClicked :: forall a. ClickCheck -> CanvasMouseEvent -> Geometry a -> Boolean
isClicked MouseInside { position } geometry = pointInside geometry position
isClicked (MouseCloserThan amount) event geometry = 
    isClicked MouseInside event geometry || distanceToShape geometry event.position < amount

distanceToShape :: forall a. Geometry a -> Vec2 -> Number
distanceToShape geometry point = distance point (closestPoint geometry point)

distanceToShapeSquared :: forall a. Geometry a -> Vec2 -> Number
distanceToShapeSquared geometry point = distanceSquared point (closestPoint geometry point)

---------- Foreign imports
foreign import _rect :: forall a. ForeignGeometryConstructor RectAttributes a
foreign import _circle :: forall a. ForeignGeometryConstructor CircleAttributes a
foreign import _group :: forall a. ForeignGeometryConstructor GroupAttributes a
foreign import _text :: forall a. ForeignGeometryConstructor 
    (OptionalTextAttributes <+> TextAttributes) a

foreign import render :: forall a. Context2D -> Geometry a -> Effect Unit
foreign import attributes :: forall a. Geometry a -> Record (IncompleteGeometryAttributes a)
foreign import children :: forall a. Geometry a -> Array (Geometry a)
foreign import pointInside :: forall a. Geometry a -> Vec2 -> Boolean
foreign import closestPoint :: forall a. Geometry a -> Vec2 -> Vec2
foreign import bounds :: forall a. Geometry a -> Record (AABBLike ())

