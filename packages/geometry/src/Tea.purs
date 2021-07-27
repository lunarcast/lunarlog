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
    , eventStream
    , createMouseEvent
    , absoluteBounds
    , relativeBounds
    , awaitRerender
    , currentReport
    , currentlyHovered
    ) where

import Loglude

import Data.Aged as Aged
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
import FRP.Stream as Strem
import Geoemtry.Data.AABB (AABB)
import Geometry.Base (CanvasMouseEvent, Geometry, GeometryAttributes, ReporterOutput, MultiStepRenderer, attributes, children, emptyReporterOutput, pointInside, toLocalCoordinates)
import Geometry.Base as Geometry
import Geometry.Hovered (hovered)
import Geometry.Render.Canvas (multiStepRender, render)
import Geometry.Vector (Vec2, x, y)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, setCanvasHeight, setCanvasWidth)
import Loglude.Cancelable (Cancelable)
import Loglude.Cancelable as Cancelable
import Loglude.ReactiveRef as RR
import Loglude.Run.ExternalState (EXTERNAL_STATE, runStateUsingRef)
import Prelude (Unit, identity, void)
import Run (Step(..), interpret, liftAff, on, runAccumPure, runBaseAff', send)
import Run as Run
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

data RenderF :: Type -> Type -> Type -> Type
data RenderF id action result
    = AwaitRerender result
    | CurrentReport (ReporterOutput id -> result)
    | AbsoluteBounds id (Maybe AABB -> result)
    | RelativeBounds id (Maybe AABB -> result)

type TeaResult :: forall k. k -> Type
type TeaResult state =
    { continuePropagation :: Boolean
    }

type PROPAGATION r = ( propagation :: PropagationF | r )
type RENDER id action r = ( render :: RenderF id action | r )

-- | Monad action handlers run inside
type TeaM state id action = Run (AFF + EFFECT + EXTERNAL_STATE state + PROPAGATION + RENDER id action ())

type SetupArgs :: Type -> Type -> Type
type SetupArgs s a = { propagateAction :: a -> Aff Unit }

type Tea state id action =
    { initialState :: state
    , render :: Ask Context2D => Strem.Discrete state -> Strem.Discrete (MultiStepRenderer id action)
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

absoluteBounds :: forall id action rest. id -> Run (RENDER id action rest) (Maybe AABB)
absoluteBounds id = Run.lift _render $ AbsoluteBounds id identity

relativeBounds :: forall id action rest. id -> Run (RENDER id action rest) (Maybe AABB)
relativeBounds id = Run.lift _render $ RelativeBounds id identity

awaitRerender :: forall id action rest. Run (RENDER id action rest) Unit
awaitRerender = Run.lift _render $ AwaitRerender unit

currentReport :: forall id action rest. Run (RENDER id action rest) (ReporterOutput id)
currentReport = Run.lift _render $ CurrentReport identity

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
-- | Get a stack of stuff a position hovers over using the current indexed report
currentlyHovered :: forall state action id. Ask Context2D => Hashable id => HashSet id -> Vec2 -> TeaM state id action (Array id) 
currentlyHovered except position = currentReport <#> hovered except position

runTea :: 
    forall state id action. 
    Hashable id => 
    Ref state -> 
    (state -> Effect Unit) -> 
    Stream.Notifier -> 
    Ref (ReporterOutput id) -> 
    TeaM state id action Unit -> 
    Aff (TeaResult state)
runTea rawState syncState rerenders reports = runRender >>> runStateUsingRef rawState >>> runPropagation >>> map fst >>> runBaseAff'
    where
    runRender = interpret (on _render handleRender send)
    runPropagation = runAccumPure (\current -> on _propagation (handlePropagation current >>> Loop) Done) Tuple { continuePropagation: true }

    handlePropagation :: _ -> PropagationF ~> Tuple _
    handlePropagation old (StopPropagation next) = old { continuePropagation = false } /\ next

    handleRender :: forall r. RenderF id action ~> Run (AFF + EXTERNAL_STATE state + EFFECT r)
    handleRender (AbsoluteBounds id continue) = liftEffect (Ref.read reports) <#> \report -> continue (HashMap.lookup id report.absoluteBounds)
    handleRender (RelativeBounds id continue) = liftEffect (Ref.read reports) <#> \report -> continue (HashMap.lookup id report.relativeBounds)
    handleRender (CurrentReport continue) = liftEffect (Ref.read reports) <#> continue 
    handleRender (AwaitRerender next) = ado
        liftEffect (Ref.read rawState >>= syncState)
        liftAff $ Cancelable.pull rerenders.event
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
    rawState <- liftEffect $ Ref.new tea.initialState
    state <- liftEffect Stream.create
    rerenders <- liftEffect Stream.notifier
    renderer <- liftEffect $ Ref.new ((0 /\ Geometry.None zero) /\ []) 
    geometry <- liftEffect $ Ref.new (Geometry.None zero)

    let 
        renderStream :: Stream.Discrete (MultiStepRenderer id action)
        renderStream = tea.render (state.event # Aged.dropDuplicates)

    let propagateAction action = ado
          result <- runTea rawState state.push rerenders indexedReport $ tea.handleAction action
          in result.continuePropagation

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

    let handleResize size = do
          canvasElement <- contextToCanvas ask
          setCanvasWidth canvasElement $ x size
          setCanvasHeight canvasElement $ y size
          Ref.write true dirty

    let loop = const do
          Ref.read rawState >>= state.push
          Ref.read dirty >>= flip when do
            size <- RR.read windowSize
            clearRect ask { x: 0.0, y: 0.0, width: x size, height: y size }
            Ref.read renderer <#> multiStepRender >>= \(thisGeometry /\ report) -> do
                render ask thisGeometry
                Ref.write report indexedReport
                Ref.write thisGeometry geometry
            Ref.write false dirty
            Stream.notify rerenders

    Cancelable.subscribe raf loop
    Cancelable.subscribe renderStream \current -> do
        Ref.write current renderer
        Ref.write true dirty
    Cancelable.subscribe clicks \ev -> do
        currentGeometry <- Ref.read geometry
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onClick $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe mouseup \ev -> do
        currentGeometry <- Ref.read geometry
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onMouseup $ createMouseEvent ev) currentGeometry
    Cancelable.subscribe mousedown \ev -> do
        currentGeometry <- Ref.read geometry
        propagateActions $ dispatchEvent identity (checkMouseEvents _.onMousedown $ createMouseEvent ev) currentGeometry

    Cancelable.subscribe (RR.changes windowSize) handleResize

    liftEffect (RR.read windowSize >>= handleResize)
    tea.setup { propagateAction: propagateAction >>> void }
    liftEffect $ state.push tea.initialState
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

---------- Foreign imports
foreign import windowSize :: ReadableRef Vec2
foreign import contextToCanvas :: Context2D -> Effect CanvasElement