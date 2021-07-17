module Geometry.Tea 
    ( SetupArgs
    , Tea
    , TeaM
    , RENDER
    , PROPAGATION
    , RenderF
    , PropagationF
    , launchTea
    , stopPropagation
    , currentGeometry
    , eventStream
    , createMouseEvent
    , absoluteBounds
    , relativeBounds
    , awaitRerender
    ) where

import Loglude

import Data.Array as Array
import Data.HashMap as HashMap
import Data.MouseButton (MouseButtons(..))
import Data.Undefined.NoProblem as Opt
import Data.ZipperArray as ZipperArray
import Data.ZipperArray as Zipperrry
import Effect.Aff (launchAff_)
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geoemtry.Data.AABB (AABB)
import Geometry.Base (CanvasMouseEvent, Geometry, GeometryAttributes, attributes, children, pointInside, toLocalCoordinates)
import Geometry.Render.Canvas (ReporterOutput, emptyReporterOutput, render)
import Graphics.Canvas (Context2D, clearRect)
import Loglude.Cancelable (Cancelable)
import Loglude.Cancelable as Cancelable
import Loglude.ReactiveRef (pushAndWait)
import Loglude.ReactiveRef as RR
import Prelude (Unit, identity, void)
import Record as Record
import Run (Step(..), interpret, liftAff, on, runAccumPure, runBaseAff', send)
import Run as Run
import Run.State (_state, execState, get)
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mouseup, click) as EventType

---------- Types
newtype EventCheckGenerator id action = EventCheckGenerator (forall zoom. Geometry id zoom -> EventChecker id action)
type EventChecker id action = (forall zoom. ((zoom -> action) -> Geometry id zoom -> Maybe action) /\ EventCheckGenerator id action)

data PropagationF result
    = StopPropagation result

data RenderF id action result
    = AwaitRerender result
    | CurrentGeometry (Geometry id action -> result)
    | AbsoluteBounds id (Maybe AABB -> result)
    | RelativeBounds id (Maybe AABB -> result)

type TeaResult state =
    { state :: state
    , continuePropagation :: Boolean
    }

type PROPAGATION r = ( propagation :: PropagationF | r )
type RENDER id action r = ( render :: RenderF id action | r )

-- | Monad action handlers run inside
type TeaM state id action = Run (AFF + EFFECT + STATE state + PROPAGATION + RENDER id action ())

type SetupArgs :: Type -> Type -> Type
type SetupArgs s a = { propagateAction :: a -> Aff Unit }

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
stopPropagation :: forall rest. Run (PROPAGATION rest) Unit
stopPropagation = Run.lift _propagation $ StopPropagation unit

currentGeometry :: forall id action rest. Run (RENDER id action rest) (Geometry id action)
currentGeometry = Run.lift _render $ CurrentGeometry identity

absoluteBounds :: forall id action rest. id -> Run (RENDER id action rest) (Maybe AABB)
absoluteBounds id = Run.lift _render $ AbsoluteBounds id identity

relativeBounds :: forall id action rest. id -> Run (RENDER id action rest) (Maybe AABB)
relativeBounds id = Run.lift _render $ RelativeBounds id identity

awaitRerender :: forall id action rest. Run (RENDER id action rest) Unit
awaitRerender = Run.lift _render $ AwaitRerender unit

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
runTea :: 
    forall state id action. 
    Hashable id => 
    WriteableRef state -> 
    Stream.Discrete Unit -> 
    Ref (ReporterOutput id) -> 
    state -> 
    Geometry id action -> 
    TeaM state id action Unit -> 
    Aff (TeaResult state) 
runTea pushState rerenders reports initialState geometry = runRender >>> execState initialState >>> runPropagation >>> map asRecord >>> runBaseAff'
    where
    asRecord = uncurry $ flip $ Record.insert _state

    runRender = interpret (on _render handleRender send)

    runPropagation = runAccumPure (\current -> on _propagation (handlePropagation current >>> Loop) Done) Tuple { continuePropagation: true }

    handlePropagation :: _ -> PropagationF ~> Tuple _
    handlePropagation old (StopPropagation next) = old { continuePropagation = false } /\ next

    handleRender :: forall r. RenderF id action ~> Run (AFF + EFFECT + STATE state r)
    handleRender (CurrentGeometry continue) = pure $ continue geometry
    handleRender (AbsoluteBounds id continue) = liftEffect (Ref.read reports) <#> \report -> continue (HashMap.lookup id report.absoluteBounds)
    handleRender (RelativeBounds id continue) = liftEffect (Ref.read reports) <#> \report -> continue (HashMap.lookup id report.relativeBounds)
    handleRender (AwaitRerender next) = ado
        get >>= flip pushAndWait pushState >>> liftAff
        liftAff $ Cancelable.pull rerenders
        in next

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
launchTea :: forall state id action. Hashable id => Ask Context2D => Tea state id action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    indexedReport <- liftEffect $ Ref.new emptyReporterOutput
    state <- liftEffect $ RR.writeable tea.initialState
    rerenders <- liftEffect $ Stream.create

    let 
        renderStream :: ReadableRef (Geometry id action)
        renderStream = tea.render (RR.readonly state)

    let propagateAction action = do 
          currentState <- liftEffect $ RR.read state
          currentGeometry <- liftEffect $ RR.read renderStream
          result <- runTea state rerenders.event indexedReport currentState currentGeometry $ tea.handleAction action
          liftEffect $ RR.write result.state state
          pure result.continuePropagation

    let 
      propagateActions :: Array action -> Effect Unit
      propagateActions = ZipperArray.fromArray >>> maybe (pure unit) propagateActionsImpl >>> launchAff_

      propagateActionsImpl :: ZipperArray action -> Aff Unit
      propagateActionsImpl actions = do
          let current = ZipperArray.current actions 
          shouldContinue <- propagateAction current 
          case ZipperArray.goNext actions of
            Just continue | shouldContinue -> propagateActionsImpl continue
            _ -> pure unit

    let loop = const do
          Ref.read dirty >>= flip when do
            clearRect ask { x: 0.0, y: 0.0, width: 1000.0, height: 1000.0 }
            RR.read renderStream >>= render ask >>= flip Ref.write indexedReport
            Ref.write false dirty
            rerenders.push unit

    Cancelable.subscribe raf loop
    Cancelable.subscribe (RR.changes renderStream) \_ -> do
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
_render :: Proxy "render"
_render = Proxy

_propagation :: Proxy "propagation"
_propagation = Proxy

---------- Typeclass instances
derive instance Functor PropagationF
derive instance Functor (RenderF id action)
