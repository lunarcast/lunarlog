module Geometry.Tea 
    ( SetupArgs
    , Tea
    , TeaM
    , TEA
    , TeaF
    , launchTea
    , stopPropagation
    , currentGeometry
    , eventStream
    , createMouseEvent
    ) where

import Loglude

import Data.Array as Array
import Data.MouseButton (MouseButtons(..))
import Data.Undefined.NoProblem as Opt
import Data.ZipperArray as ZipperArray
import Data.ZipperArray as Zipperrry
import Debug (traceM)
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
newtype EventCheckGenerator id action = EventCheckGenerator (forall zoom. Geometry id zoom -> EventChecker id action)
type EventChecker id action = (forall zoom. ((zoom -> action) -> Geometry id zoom -> Maybe action) /\ EventCheckGenerator id action)

data TeaF id action result
    = StopPropagation result
    | CurrentGeometry (Geometry id action -> result)

type TeaResult state =
    { state :: state
    , continuePropagation :: Boolean
    }

type TEA id action r = ( tea :: TeaF id action | r )

-- | Monad action handlers run inside
type TeaM state id action = Run (EFFECT + STATE state + TEA id action ())

type SetupArgs :: Type -> Type -> Type
type SetupArgs s a = { propagateAction :: a -> Effect Unit }

type Tea state id action =
    { initialState :: state
    , render :: Ask Context2D => ReadableRef state -> ReadableRef (Geometry id action)
    , handleAction :: action -> TeaM state id action Unit
    , setup :: SetupArgs state action -> Cancelable Unit
    }

data CanvasEvent
    = Click CanvasMouseEvent
    | MouseDown CanvasMouseEvent
    | MouseUp CanvasMouseEvent
    | MouseMove CanvasMouseEvent

---------- Constructors
currentGeometry :: forall id action result. Run (TEA id action result) (Geometry id action)
currentGeometry = Run.lift _tea $ CurrentGeometry identity

stopPropagation :: forall id action result. Run (TEA id action result) Unit
stopPropagation = Run.lift _tea $ StopPropagation unit

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
runTea :: forall state id action. state -> Geometry id action -> TeaM state id action Unit -> Effect (TeaResult state) 
runTea state geometry = execState state >>> runTea >>> map asRecord >>> runBaseEffect
    where
    asRecord = uncurry $ flip $ Record.insert _state
    runTea = runAccumPure (\current -> on _tea (handler current >>> Loop) Done) Tuple { continuePropagation: true }

    handler :: _ -> TeaF id action ~> Tuple _
    handler old (StopPropagation next) = old { continuePropagation = false } /\ next
    handler propagation (CurrentGeometry continue) = propagation /\ continue geometry

checkMouseEvents :: 
    forall id action. 
    Ask Context2D => 
    (forall rest zoom. Record (GeometryAttributes id zoom rest) -> Opt (CanvasMouseEvent -> zoom)) -> 
    CanvasMouseEvent -> 
    EventChecker id action
checkMouseEvents key event = result    
    where
    result :: EventChecker id action
    result = check /\ EventCheckGenerator recurse

    check :: forall zoom. (zoom -> action) -> Geometry id zoom -> Maybe action
    check unzoom geom = case Opt.toMaybe $ attributes geom Opt.undefined key of
        Just handler | pointInside event.localPosition geom -> Just $ unzoom $ handler event
        _ -> Nothing

    recurse :: forall zoom. Geometry id zoom -> EventChecker id action
    recurse geometry = do
        let newEvent = event { localPosition = toLocalCoordinates geometry event.localPosition }
        checkMouseEvents key newEvent


handleActions :: forall action. (action -> Effect Boolean) -> ZipperArray action -> Effect Unit
handleActions propagateAction zipper = do
    continuePropagation <- propagateAction (ZipperArray.current zipper)
    when continuePropagation do
        for_ (Zipperrry.goNext zipper) $ handleActions propagateAction

---------- Implementations
launchTea :: forall state id action. Ask Context2D => Tea state id action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    state <- liftEffect $ RR.writeable tea.initialState
    let 
        renderStream :: ReadableRef (Geometry id action)
        renderStream = tea.render (RR.readonly state)

    let propagateAction action = do 
          currentState <- RR.read state
          currentGeometry <- RR.read renderStream
          result <- runTea currentState currentGeometry $ tea.handleAction action
          RR.write result.state state
          pure result.continuePropagation

    let propagateActions actions = for_ (ZipperArray.fromArray actions) $ handleActions propagateAction

    let loop = const do
          Ref.read dirty >>= flip when do
            clearRect ask { x: 0.0, y: 0.0, width: 1000.0, height: 1000.0 }
            RR.read renderStream >>= render ask
            Ref.write false dirty

    Cancelable.subscribe raf loop
    Cancelable.subscribe (RR.changes renderStream) \_ -> do
        traceM "rerender"
        Ref.write true dirty
    Cancelable.subscribe mousedown \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onClick $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe mouseup \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onMouseup $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe clicks \ev -> do
        currentGeometry <- RR.read renderStream
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onMousedown $ createMouseEvent ev) currentGeometry

    tea.setup { propagateAction: propagateAction >>> void }
    where
    clicks = eventStream MouseEvent.fromEvent EventType.click
    mousedown = eventStream MouseEvent.fromEvent EventType.mousedown
    mouseup = eventStream MouseEvent.fromEvent EventType.mouseup

    raf :: Stream.Discrete Unit
    raf = animationFrame


dispatchEvent :: forall id action zoomed. (zoomed -> action) -> EventChecker id action -> Geometry id zoomed -> Array action
dispatchEvent unzoom (check /\ EventCheckGenerator recurse) geometry = [Array.last nested, current <#> pure] >>= (fromMaybe [])
    where
    current = check unzoom geometry
    continue = recurse geometry
    nested = children geometry (\focus child -> dispatchEvent (focus >>> unzoom) continue child)
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
derive instance Functor (TeaF id action)
