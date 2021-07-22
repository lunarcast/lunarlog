-- | Definitions for the core of the geometry package
module Geometry.Base where

import Loglude

import Data.Array as Array
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Lens (traversed)
import Data.MouseButton (MouseButtons)
import Data.Undefined.NoProblem.Closed as Closed
import Geoemtry.Data.AABB (AABBLike, AABB)
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

newtype MapActionF id to from = MapActionF 
    { target :: Geometry id from
    , map :: from -> to
    }

data Geometry :: Type -> Type -> Type
data Geometry id action
    = Circle (Record (CircleAttributes id action + GeometryAttributes id action ()))
    | Rect (Record (RectAttributes id action + GeometryAttributes id action ()))
    | Group (Record (GroupAttributes id action + GeometryAttributes id action ()))
    | Text (Record (TextAttributes id action + GeometryAttributes id action ()))
    | Transform (Record (TransformAttributes id action + GeometryAttributes id action ()))
    | LockBounds (Record (LockBoundsAttributes id action ()))
    | MapAction (Exists (MapActionF id action))
    | Reporter (Record (ReporterAttributes Id id action ()))
    | None Vec2

data ClickCheck
    = MouseInside
    | MouseCloserThan Number

type Attributes = Type -> Type -> Row Type -> Row Type

type EventAttributes :: Attributes
type EventAttributes id action r =
    ( onClick :: Opt (EventHandler CanvasMouseEvent action)
    , onMousedown :: Opt (EventHandler CanvasMouseEvent action) 
    , onMouseup :: Opt (EventHandler CanvasMouseEvent action)  
    , onMouseMove :: Opt (EventHandler CanvasMouseEvent action)
    | r )

type GeometryAttributes :: Attributes
type GeometryAttributes id action r =
    ( fill :: Opt String
    , stroke :: Opt String
    , weight :: Opt Number
    , alpha :: Opt Number

    -- Here for debugging only (rn)
    , label :: Opt String
    | EventAttributes id action r )

---------- Attribute types for individual shapes
type RectAttributes :: Attributes
type RectAttributes id a r = AABBLike r

type CircleAttributes :: Attributes
type CircleAttributes id a r = ( position :: Vec2, radius :: Number | r )

type GroupAttributes :: Attributes
type GroupAttributes id action r = ( children :: Array (Geometry id action) | r )

type TextAttributes :: Attributes
type TextAttributes id a r = 
    ( position :: Vec2
    , text :: String
    , baseline :: Opt TextBaseline
    , font :: Opt String 
    | r )

type TransformAttributes :: Attributes
type TransformAttributes id action r =
    ( transform :: TransformMatrix
    , target :: Geometry id action
    | r )

type LockBoundsAttributes :: Attributes
type LockBoundsAttributes id action r =
    ( bounds :: Lazy (Maybe AABB)
    , target :: Geometry id action
    | r )

type ReporterAttributes :: (Type -> Type) -> Attributes
type ReporterAttributes f id action r =
    ( id :: id
    , target :: Geometry id action
    , reportAbsoluteBounds :: f Boolean
    , reportRelativeBounds :: f Boolean
    | r )

---------- Constructor types
type GeometryConstructor extra = 
    forall given action id.
    Closed.Coerce given (Record (extra id action + GeometryAttributes id action ())) => 
    given -> Geometry id action

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

lockBounds :: forall id action. Ask Context2D => (Geometry id action -> Geometry id action) -> Geometry id action -> Geometry id action
lockBounds f geometry = LockBounds { target: f geometry, bounds: Lazy.defer \_ -> bounds geometry }

text :: GeometryConstructor TextAttributes
text = Closed.coerce >>> Text

mapAction :: forall id from to. (from -> to) -> Geometry id from -> Geometry id to
mapAction map target = { target, map } # MapActionF # mkExists # MapAction

reporter :: forall given action id. Closed.Coerce given (Record (ReporterAttributes Opt id action ())) => given -> Geometry id action
reporter = Closed.coerce >>> withDefaults >>> Reporter
    where
    withDefaults :: Record (ReporterAttributes Opt id action ()) -> _
    withDefaults incomplete =
        { target: incomplete.target
        , id: incomplete.id
        , reportAbsoluteBounds: Opt.fromOpt false incomplete.reportAbsoluteBounds
        , reportRelativeBounds: Opt.fromOpt false incomplete.reportRelativeBounds
        }

annotate :: forall id action. id -> Geometry id action -> Geometry id action
annotate id target = reporter { id, target }

---------- Helpers
translate :: forall id action. Vec2 -> Geometry id action -> Geometry id action
translate amount (Circle attributes) = 
    Circle $ over _position ((+) amount) attributes
translate amount (Rect attributes) =
    Rect $ over _position ((+) amount) attributes
translate amount (Text attributes) =
    Text $ over _position ((+) amount) attributes
translate amount (Group attributes) =
    Group $ over (_children <<< traversed) (translate amount) attributes
translate amount (Reporter attributes) =
    Reporter $ over _target (translate amount) attributes
translate amount (Transform attributes) =
    Transform $ over _transform (_ <> Transform.translate amount) attributes
translate amount (LockBounds attributes) = LockBounds 
    { target: translate amount attributes.target
    , bounds: attributes.bounds <#> over (_Just <<< _position) ((+) amount)
    } 
translate amount (MapAction existential) = MapAction $ mapExists mapInner existential
    where
    mapInner :: MapActionF id action ~> MapActionF id action
    mapInner = over (_Newtype <<< _target) $ translate amount 
translate amount (None position) = None (position + amount)

-- | Calculate the minimum rectangle needed to surround the shape
bounds :: forall id action. Ask Context2D => Geometry id action -> Maybe AABB
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
        Just a, Just b -> Just $ AABB.union a b
        a, b -> a <|> b
bounds (LockBounds attributes) = Lazy.force attributes.bounds
bounds (Transform attributes) = do
    innerBounds <- bounds attributes.target
    AABB.fromPoints $ points $ Transform attributes
bounds (Text attributes) = Just do
    let metrics = measureText ask attributes.font attributes.text 
    { position: attributes.position
    , size: vec2 metrics.width (metrics.fontBoundingBoxAscent)
    }
bounds (MapAction inner) = inner # runExists (unwrap >>> _.target >>> bounds)
bounds (Reporter { target }) = bounds target
bounds (None position) = Just { position, size: zero }

-- | Returns a polygon surrounding the shape
points :: forall id action. Ask Context2D => Geometry id action -> Array Vec2
points (Transform { transform, target }) = points target <#> multiplyVector transform
points (Reporter { target }) = points target
points a = bounds a # maybe [] AABB.points

pointInside :: forall id action. Ask Context2D => Vec2 -> Geometry id action -> Boolean
pointInside point (Circle attributes) = 
    distanceSquared point attributes.position < attributes.radius `pow` 2.0
pointInside point (Group { children }) = Array.any (pointInside point) children
pointInside point (LockBounds { target }) = pointInside point target
pointInside point shape@(Transform { target }) = pointInside projected target
    where
    projected = toLocalCoordinates shape point
pointInside point shape = bounds shape # maybe false (AABB.pointInside point)

-- TODO: find a way to cache the inverse
toLocalCoordinates :: forall id action. Geometry id action -> Vec2 -> Vec2
toLocalCoordinates (Transform { target, transform }) point = multiplyVector (inverse transform) point
toLocalCoordinates _ point = point

-- | Get an array with all the children owned by a geometry
children :: forall id action. Geometry id action -> forall result. (forall subaction. (subaction -> action) -> Geometry id subaction -> result) -> Array result
children (Group { children }) f = f identity <$> children
children (Reporter { target }) f = [f identity target]
children (Transform { target }) f = [f identity target]
children (LockBounds { target }) f = [f identity target]
children (MapAction existential) f = [existential # runExists \(MapActionF { target, map }) -> f map target]
children _ _ = []

-- | Get an existential with the attributes carried around by a geometry
attributes :: forall id action result. Geometry id action -> result -> (forall r. Record (GeometryAttributes id action r) -> result) -> result
attributes (Circle attributes) _ f = f attributes
attributes (Rect attributes) _ f = f attributes
attributes (Group attributes) _ f = f attributes
attributes (Text attributes) _ f = f attributes
attributes (Transform attributes) _ f = f attributes
attributes (None position) _ f = f $ (Closed.coerce {} :: Record (GeometryAttributes id action ()))
attributes _ default _ = default

---------- Lenses
_position :: forall r. Lens' { position :: Vec2 | r } Vec2
_position = prop (Proxy :: _ "position")

_children :: forall id action r. Lens' { children :: Array (Geometry id action) | r } (Array (Geometry id action))
_children = prop (Proxy :: _ "children")

_transform :: forall r. Lens' { transform :: TransformMatrix | r } TransformMatrix
_transform = prop (Proxy :: _ "transform")

_target :: forall id action r. Lens' { target :: Geometry id action | r } (Geometry id action)
_target = prop (Proxy :: _ "target")

---------- Foreign imports
foreign import measureText :: Context2D -> Opt String -> String -> TextMetrics

---------- Typeclass instances
derive instance Newtype (MapActionF id to from) _