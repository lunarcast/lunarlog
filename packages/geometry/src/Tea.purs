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
    , eventStream
    , createMouseEvent
    ) where

import Loglude

import Data.Array as Array
import Data.MouseButton (MouseButtons(..))
import Data.Undefined.NoProblem as Opt
import Data.ZipperArray as ZipperArray
import Data.ZipperArray as Zipperrry
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Base (CanvasMouseEvent, Geometry, GeometryAttributes, attributes, children, pointInside, toLocalCoordinates)
import Geometry.Render.Canvas (render)
import Graphics.Canvas (Context2D, clearRect)
import Loglude.Cancelable (Cancelable)
import Loglude.Cancelable as Cancelable
import Loglude.ReactiveRef as RR
import Record as Record
import Run (Step(..), on, runAccumPure, runBaseEffect)
import Run as Run
import Run.State (_state, execState)
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mouseup, click) as EventType

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
    , render :: Ask Context2D => ReadableRef state -> ReadableRef (Geometry action)
    , handleAction :: action -> TeaM state action Unit
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

createMouseEvent :: MouseEvent -> CanvasMouseEvent
createMouseEvent ev = 
    { buttons: MouseButtons $ MouseEvent.buttons ev
    , worldPosition: position 
    , localPosition: position 
    }
    where
    position = toNumber <$> vec2  
        (MouseEvent.clientX ev) 
        (MouseEvent.clientY ev)

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

checkMouseEvents :: 
    forall action. 
    Ask Context2D => 
    (forall r. Record (GeometryAttributes action r) -> Opt (CanvasMouseEvent -> action)) -> 
    CanvasMouseEvent -> 
    EventChecker action
checkMouseEvents key event = check /\ EventCheckGenerator \geometry -> do
    let newEvent = event { localPosition = toLocalCoordinates geometry event.localPosition }
    checkMouseEvents key newEvent
    where
    check geom = case Opt.toMaybe $ attribs key of
        Just handler | pointInside event.localPosition geom -> Just $ handler event
        _ -> Nothing
        where
        attribs = attributes geom 

handleActions :: forall action. (action -> Effect Boolean) -> ZipperArray action -> Effect Unit
handleActions propagateAction zipper = do
    continuePropagation <- propagateAction (ZipperArray.current zipper)
    when continuePropagation do
        for_ (Zipperrry.goNext zipper) $ handleActions propagateAction

---------- Implementations
launchTea :: forall state action. Ask Context2D => Tea state action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    shouldRecompute <- liftEffect $ Ref.new true
    state <- RR.writeable tea.initialState
    let 
        renderStream :: Ask Context2D => ReadableRef (Geometry action)
        renderStream = tea.render (RR.readonly state)

    let propagateAction action = do 
          currentState <- RR.read state
          currentGeometry <- RR.read renderStream
          result <- runTea currentState currentGeometry $ tea.handleAction action
          RR.write result.state state
          Ref.write true dirty
          pure result.continuePropagation

    let propagateActions actions = for_ (ZipperArray.fromArray actions) $ handleActions propagateAction

    let loop = const do
          Ref.read dirty >>= flip when do
            clearRect ask { x: 0.0, y: 0.0, width: 1000.0, height: 1000.0 }
            RR.read renderStream >>= render ask

            Ref.write false dirty

    Cancelable.subscribe raf loop
    Cancelable.subscribe (RR.changes renderStream) \_ -> do
        Ref.write true dirty
    Cancelable.subscribe mousedown \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent (checkMouseEvents _.onClick $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe mouseup \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent (checkMouseEvents _.onMouseup $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe clicks \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent (checkMouseEvents _.onMousedown $ createMouseEvent ev) currentGeometry

    tea.setup { propagateAction: propagateAction >>> void }
    liftEffect $ Ref.write true dirty
    where
    clicks = eventStream MouseEvent.fromEvent EventType.click
    mousedown = eventStream MouseEvent.fromEvent EventType.mousedown
    mouseup = eventStream MouseEvent.fromEvent EventType.mouseup

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
