module Geometry.Types where

import Loglude

import Graphics.Canvas (Context2D)
import Prim.Row (class Union)

data Geometry :: Type -> Type
data Geometry a

type Id :: forall k. k -> k
type Id a = a

type Vec2 = Vec D2 Int

type GenericGeometryAttributes :: forall k. (Type -> k) -> Type -> Row k
type GenericGeometryAttributes f action = 
    ( fill :: f String
    , onClick :: f (CanvasMouseEvent -> action) 
    )

type GeometryAttributes a = GenericGeometryAttributes Id a
type IncompleteGeometryAttributes a = GenericGeometryAttributes Opt a

type GeometryConstructor required action
    = forall given rest.
    Union given rest (GeometryAttributes action) => 
    Record (required given action) -> Geometry action

type ForeignGeometryConstructor required action
    = Record (required (GeometryAttributes action) action) -> Geometry action

type RequiredAttributes = Row Type -> Type -> Row Type

type RectAttributes :: RequiredAttributes
type RectAttributes r a = ( position :: Vec2, size :: Vec2 | r )

type CircleAttributes :: RequiredAttributes
type CircleAttributes r a = ( position :: Vec2, radius :: Int | r )

type GroupAttributes :: RequiredAttributes
type GroupAttributes r a = ( children :: Array (Geometry a) | r )

type CanvasMouseEvent = { buttons :: Int, position :: Vec2 }

---------- Constructors
group :: forall a. GeometryConstructor GroupAttributes a 
group = unsafeCoerce _group

rect :: forall a. GeometryConstructor RectAttributes a
rect = unsafeCoerce _rect

circle :: forall a. GeometryConstructor CircleAttributes a
circle = unsafeCoerce _circle

none :: forall a. Geometry a 
none = group { children: [] }

---------- Foreign imports
foreign import _rect :: forall a. ForeignGeometryConstructor RectAttributes a
foreign import _circle :: forall a. ForeignGeometryConstructor CircleAttributes a
foreign import _group :: forall a. ForeignGeometryConstructor GroupAttributes a

foreign import render :: forall a. Context2D -> Geometry a -> Effect Unit
foreign import attributes :: forall a. Geometry a -> Record (IncompleteGeometryAttributes a)
foreign import children :: forall a. Geometry a -> Array (Geometry a)
foreign import pointInside :: forall a. Geometry a -> Vec2 -> Boolean