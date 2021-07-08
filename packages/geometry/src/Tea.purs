module Geometry.Tea 
    ( SetupArgs
    , Tea
    , TeaM
    , TEA
    , TeaF
    , launchTea
    , stopPropagation
    , currentGeometry
    , shouldRecompute
    ) where

import Loglude

import Data.Array as Array
import Data.Undefined.NoProblem as Opt
import Data.ZipperArray as ZipperArray
import Data.ZipperArray as Zipperrry
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Base (CanvasMouseEvent, Geometry, IncompleteGeometryAttributes, attributes, render)
import Geometry.Base as Geometry
import Geometry.Hiccup (children, pointInside, toLocalCoordinates)
import Graphics.Canvas (Context2D, clearRect)
import Loglude.Cancelable as Cancelable
import Prim.Row as Row
import Record as Record
import Run (Step(..), on, runAccumPure, runBaseEffect)
import Run as Run
import Run.State (_state, execState)
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.Event.EventTypes as EventType
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

---------- Types
newtype EventCheckGenerator a = EventCheckGenerator (Geometry a -> EventChecker a)
type EventChecker a = (Geometry a -> Maybe a) /\ EventCheckGenerator a

data TeaF action result
    = StopPropagation result
    | ShouldRecompute result 
    | CurrentGeometry (Geometry action -> result)

type TeaResult state =
    { state :: state
    , continuePropagation :: Boolean
    , shouldRecompute :: Boolean
    }

type TEA action r = ( tea :: TeaF action | r )

-- | Monad action handlers run inside
type TeaM state action = Run (EFFECT + STATE state + TEA action ())

type SetupArgs :: Type -> Type -> Type
type SetupArgs s a = { propagateAction :: a -> Effect Unit }

type Tea state action =
    { initialState :: state
    , render :: Ask Context2D => state -> Geometry action
    , handleAction :: action -> TeaM state action Unit
    , context :: Context2D
    , setup :: SetupArgs state action -> Cancelable Unit
    }

data CanvasEvent
    = Click CanvasMouseEvent
    | MouseDown CanvasMouseEvent
    | MouseUp CanvasMouseEvent
    | MouseMove CanvasMouseEvent

---------- Constructors
currentGeometry :: forall action result. Run (TEA action result) (Geometry action)
currentGeometry = Run.lift _tea $ CurrentGeometry identity

stopPropagation :: forall action result. Run (TEA action result) Unit
stopPropagation = Run.lift _tea $ StopPropagation unit

shouldRecompute :: forall action result. Run (TEA action result) Unit
shouldRecompute = Run.lift _tea $ ShouldRecompute unit

---------- Helpers
runTea :: forall state action. state -> Geometry action -> TeaM state action Unit -> Effect (TeaResult state) 
runTea state geometry = execState state >>> runTea >>> map asRecord >>> runBaseEffect
    where
    asRecord = uncurry $ flip $ Record.insert _state
    runTea = runAccumPure (\current -> on _tea (handler current >>> Loop) Done) Tuple { shouldRecompute: false, continuePropagation: true }

    handler :: _ -> TeaF action ~> Tuple _
    handler old (StopPropagation next) = old { continuePropagation = false } /\ next
    handler old (ShouldRecompute next) = old { shouldRecompute = true } /\ next
    handler propagation (CurrentGeometry continue) = propagation /\ continue geometry

checkGenerator :: forall action key remaining.
    Row.Cons key (Opt (CanvasMouseEvent -> action)) remaining (IncompleteGeometryAttributes action) => 
    IsSymbol key => 
    Proxy key -> CanvasMouseEvent -> EventChecker action
checkGenerator key event = check /\ EventCheckGenerator \geometry -> do
    let newEvent = recurse geometry event
    checkGenerator key newEvent
    where
    recurse parent event = event { position = toLocalCoordinates parent event.position }

    check geom = case Opt.toMaybe $ Record.get key attribs of
        Just handler | pointInside event.position geom -> Just $ handler event
        _ -> Nothing
        where
        attribs = attributes geom 

---------- Implementations
launchTea :: forall state action. Tea state action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    shouldRecompute <- liftEffect $ Ref.new true
    state <- liftEffect $ Ref.new tea.initialState
    geometry <- liftEffect $ Ref.new $ Geometry.group { children: [] }

    let propagateAction action = do 
          currentState <- Ref.read state
          currentGeometry <- Ref.read geometry
          result <- runTea currentState currentGeometry $ tea.handleAction action
          Ref.write result.state state
          Ref.write true dirty
          when result.shouldRecompute $ Ref.write true shouldRecompute
          pure result.continuePropagation

    let loop = const do
          Ref.read shouldRecompute >>= flip when do
            thisState <- Ref.read state

            let currentGeometry = provide tea.context $ tea.render thisState

            Ref.write currentGeometry geometry
            Ref.write false shouldRecompute

          Ref.read dirty >>= flip when do
            clearRect tea.context { x: 0.0, y: 0.0, width: 1000.0, height: 1000.0 }
            Ref.read geometry >>= render tea.context

            Ref.write false dirty

    Cancelable.subscribe raf loop
    Cancelable.subscribe clicks \ev -> do
        currentGeometry <- Ref.read geometry

        let event :: CanvasMouseEvent
            event = 
                { buttons: MouseEvent.buttons ev
                , position: toNumber <$> vec2  
                    (MouseEvent.clientX ev) 
                    (MouseEvent.clientY ev) 
                }

            actions = dispatchEvent (checkGenerator (Proxy :: _ "onClick") event) currentGeometry

        let 
          go :: ZipperArray action -> Effect Unit
          go zipper = do
            continuePropagation <- propagateAction (ZipperArray.current zipper)
            when continuePropagation do
                for_ (Zipperrry.goNext zipper) go

        for_ (ZipperArray.fromArray actions) go

    tea.setup { propagateAction: propagateAction >>> void }
    where
    clicks :: Stream.Discrete MouseEvent
    clicks = eventStream (MouseEvent.fromEvent) EventType.click

    raf :: Stream.Discrete Unit
    raf = animationFrame


dispatchEvent :: forall a. EventChecker a -> Geometry a -> Array a
dispatchEvent (check /\ EventCheckGenerator recurse) geometry = [Array.last nested, current <#> pure] >>= (fromMaybe [])
    where
    current = check geometry
    continue = recurse geometry
    nested = next 
        <#> (\child -> dispatchEvent continue child)
        # Array.filter (Array.null >>> not)
    next = children geometry

eventStream :: forall e. (Event -> Maybe e) -> EventType -> Stream.Discrete e 
eventStream fromEvent eventType = Cancelable.createStream \emit -> do
    liftEffect (window >>= document >>= body) >>= traverse_ \body_ -> do
        listener <- liftEffect $ eventListener 
            $ fromEvent >>> traverse_ emit
        Cancelable.addEventListener eventType listener false (HtmlElement.toEventTarget body_)

---------- Proxies
_tea :: Proxy "tea"
_tea = Proxy

---------- Typeclass instances
derive instance Functor (TeaF action)