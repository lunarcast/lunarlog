-- | Definitions for the core of the geometry package
module Geometry.Base where

import Loglude

import Data.Array as Array
import Data.Lens (traversed)
import Data.MouseButton (MouseButtons)
import Data.Undefined.NoProblem.Closed as Closed
import Geoemtry.Data.AABB (AABB, AABBLike)
import Geoemtry.Data.AABB as AABB
import Geometry.TextBaseline (TextBaseline)
import Geometry.Transform (TransformMatrix, inverse, multiplyVector)
import Geometry.Transform as Transform
import Geometry.Vector (Vec2, distanceSquared)
import Graphics.Canvas (Context2D)
import Loglude as Opt
import Math (pow)

type EventHandler payload action = (payload -> action)

type CanvasMouseEvent = 
    { buttons :: MouseButtons
    , localPosition :: Vec2 
    , worldPosition :: Vec2
    }

type TextMetrics = 
    { width :: Number
    , fontBoundingBoxAscent :: Number
    }

newtype MapActionF to from = MapActionF 
    { target :: Geometry from
    , map :: from -> to
    }

data Geometry :: Type -> Type
data Geometry action
    = Circle (Record (CircleAttributes action + GeometryAttributes action ()))
    | Rect (Record (RectAttributes action + GeometryAttributes action ()))
    | Group (Record (GroupAttributes action + GeometryAttributes action ()))
    | Text (Record (TextAttributes action + GeometryAttributes action ()))
    | Transform (Record (TransformAttributes action + GeometryAttributes action ()))
    | MapAction (Exists (MapActionF action))
    | None Vec2

data ClickCheck
    = MouseInside
    | MouseCloserThan Number

type Attributes = Type -> Row Type -> Row Type

type EventAttributes :: Attributes
type EventAttributes action r =
    ( onClick :: Opt (EventHandler CanvasMouseEvent action)
    , onMousedown :: Opt (EventHandler CanvasMouseEvent action) 
    , onMouseup :: Opt (EventHandler CanvasMouseEvent action)  
    , onMouseMove :: Opt (EventHandler CanvasMouseEvent action)
    | r )

type GeometryAttributes :: Attributes
type GeometryAttributes action r =
    ( fill :: Opt String
    , stroke :: Opt String
    , weight :: Opt Number
    , alpha :: Opt Number

    -- Here for debugging only (rn)
    , label :: Opt String
    | EventAttributes action r )

---------- Attribute types for individual shapes
type RectAttributes :: Attributes
type RectAttributes a r = AABBLike r

type CircleAttributes :: Attributes
type CircleAttributes a r = ( position :: Vec2, radius :: Number | r )

type GroupAttributes :: Attributes
type GroupAttributes a r = ( children :: Array (Geometry a) | r )

type TextAttributes :: Attributes
type TextAttributes a r = 
    ( position :: Vec2
    , text :: String
    , baseline :: Opt TextBaseline
    , font :: Opt String 
    | r )

type TransformAttributes :: Attributes
type TransformAttributes a r =
    ( transform :: TransformMatrix
    , target :: Geometry a
    , transformBounds :: Opt Boolean
    | r )

---------- Constructor types
type GeometryConstructor extra = 
    forall given action. 
    Closed.Coerce given (Record (extra action + GeometryAttributes action ())) => 
    given -> Geometry action

---------- Constructors
rect :: GeometryConstructor RectAttributes
rect = Closed.coerce >>> Rect

circle :: GeometryConstructor CircleAttributes
circle = Closed.coerce >>> Circle

group :: GeometryConstructor GroupAttributes
group = Closed.coerce >>> go
    where
    go attributes@{ children } 
        | Array.null children = None zero
        | otherwise = Group attributes

transform :: GeometryConstructor TransformAttributes
transform = Closed.coerce >>> Transform

text :: GeometryConstructor TextAttributes
text = Closed.coerce >>> Text

mapAction :: forall from to. (from -> to) -> Geometry from -> Geometry to
mapAction map target = { target, map } # MapActionF # mkExists # MapAction

---------- Helpers
translate :: forall action. Vec2 -> Geometry action -> Geometry action
translate amount (Circle attributes) = 
    Circle $ over _position ((+) amount) attributes
translate amount (Rect attributes) =
    Rect $ over _position ((+) amount) attributes
translate amount (Text attributes) =
    Text $ over _position ((+) amount) attributes
translate amount (Group attributes) =
    Group $ over (_children <<< traversed) (translate amount) attributes
translate amount (Transform attributes)
    | Opt.fromOpt true attributes.transformBounds = Transform $ over _transform (_ <> Transform.translate amount) attributes
    | otherwise = Transform $ over _target (translate amount) attributes
translate amount (MapAction existential) = MapAction $ mapExists mapInner existential
    where
    mapInner :: MapActionF action ~> MapActionF action
    mapInner = over (_Newtype <<< _target) $ translate amount 
translate amount (None position) = None (position + amount)

-- | Calculate the minimum rectangle needed to surround the shape
bounds :: forall a. Ask Context2D => Geometry a -> Maybe AABB
bounds (Circle attributes) = Just
    { position: attributes.position - radius2 
    , size: ((*) 2.0) <$> radius2
    }
    where
    radius2 = vec2 attributes.radius attributes.radius
bounds (Rect { position, size }) = Just { position, size }
bounds (Group { children }) = foldr merger Nothing $ bounds <$> children
    where
    merger = case _, _ of
        Nothing, Nothing -> Nothing
        Just a, Just b -> Just $ AABB.union a b
        Just a, Nothing -> Just a
        Nothing, Just a -> Just a
bounds (Transform attributes)
    | Opt.fromOpt true attributes.transformBounds = do
        innerBounds <- bounds attributes.target
        AABB.fromPoints $ points $ Transform attributes
    | otherwise = bounds attributes.target
bounds (Text attributes) = Just do
    let metrics = measureText ask attributes.font attributes.text 
    { position: attributes.position
    , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
    }
bounds (MapAction inner) = inner # runExists (unwrap >>> _.target >>> bounds)
bounds (None position) = Just { position, size: zero }

-- | Returns a polygon surrounding the shape
points :: forall a. Ask Context2D => Geometry a -> Array Vec2
points (Transform { transform, target }) = points target <#> multiplyVector transform
points a = bounds a # maybe [] AABB.points

pointInside :: forall action. Ask Context2D => Vec2 -> Geometry action -> Boolean
pointInside point (Circle attributes) = 
    distanceSquared point attributes.position < attributes.radius `pow` 2.0
pointInside point (Group { children }) = Array.any (pointInside point) children
pointInside point shape@(Transform { target }) = pointInside projected target
    where
    projected = toLocalCoordinates shape point
pointInside point shape = bounds shape # maybe false (AABB.pointInside point)

-- TODO: find a way to cache the inverse
toLocalCoordinates :: forall action. Geometry action -> Vec2 -> Vec2
toLocalCoordinates (Transform { target, transform }) point = multiplyVector (inverse transform) point
toLocalCoordinates _ point = point

-- | Get an array with all the children owned by a geometry
children :: forall action. Geometry action -> forall result. (forall subaction. (subaction -> action) -> Geometry subaction -> result) -> Array result
children (Group { children }) f = f identity <$> children
children (Transform { target }) f = [f identity target]
children (MapAction existential) f = [existential # runExists \(MapActionF { target, map }) -> f map target]
children _ _ = []

-- | Get an existential with the attributes carried around by a geometry
attributes :: forall action result. Geometry action -> result -> (forall r. Record (GeometryAttributes action r) -> result) -> result
attributes (Circle attributes) _ f = f attributes
attributes (Rect attributes) _ f = f attributes
attributes (Group attributes) _ f = f attributes
attributes (Text attributes) _ f = f attributes
attributes (Transform attributes) _ f = f attributes
attributes (None position) _ f = f $ (Closed.coerce {} :: Record (GeometryAttributes action ()))
attributes (MapAction _) default _ = default

---------- Lenses
_position :: forall r. Lens' { position :: Vec2 | r } Vec2
_position = prop (Proxy :: _ "position")

_children :: forall a r. Lens' { children :: Array (Geometry a) | r } (Array (Geometry a))
_children = prop (Proxy :: _ "children")

_transform :: forall r. Lens' { transform :: TransformMatrix | r } TransformMatrix
_transform = prop (Proxy :: _ "transform")

_target :: forall action r. Lens' { target :: Geometry action | r } (Geometry action)
_target = prop (Proxy :: _ "target")

---------- Foreign imports
foreign import measureText :: Context2D -> Opt String -> String -> TextMetrics

---------- Typeclass instances
derive instance Newtype (MapActionF t f) _