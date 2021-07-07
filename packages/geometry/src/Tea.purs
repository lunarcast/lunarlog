module Geometry.Tea 
    ( SetupArgs
    , Tea
    , TeaM
    , TEA
    , TeaF
    , launchTea
    , stopPropagation
    , currentGeometry
    ) where

import Loglude

import Data.Array as Array
import Data.Undefined.NoProblem as Opt
import Data.ZipperArray as ZipperArray
import Data.ZipperArray as Zipperrry
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Base (CanvasMouseEvent, Geometry, attributes, children, render)
import Geometry.Base as Geometry
import Geometry.Hiccup (pointInside)
import Graphics.Canvas (Context2D, clearRect)
import Loglude.Cancelable as Cancelable
import Run (Step(..), on, runAccumPure, runBaseEffect)
import Run as Run
import Run.State (execState)
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.Event.EventTypes as EventType
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

---------- Types
data TeaF action result
    = StopPropagation result
    | CurrentGeometry (Geometry action -> result)

type TeaResult state =
    { state :: state
    , continuePropagation :: Boolean
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
stopPropagation :: forall action result. Run (TEA action result) Unit
stopPropagation = Run.lift _tea $ StopPropagation unit

currentGeometry :: forall action result. Run (TEA action result) (Geometry action)
currentGeometry = Run.lift _tea $ CurrentGeometry identity

---------- Helpers
runTea :: forall state action. state -> Geometry action -> TeaM state action Unit -> Effect (TeaResult state) 
runTea state geometry = execState state >>> runTea >>> map asRecord >>> runBaseEffect
    where
    asRecord = uncurry { continuePropagation: _, state: _ }
    runTea = runAccumPure (\current -> on _tea (handler current >>> Loop) Done) Tuple true

    handler :: Boolean -> TeaF action ~> Tuple Boolean
    handler _ (StopPropagation next) = false /\ next
    handler propagation (CurrentGeometry continue) = propagation /\ continue geometry

---------- Implementations
launchTea :: forall state action. Tea state action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    state <- liftEffect $ Ref.new tea.initialState
    geometry <- liftEffect $ Ref.new $ Geometry.group { children: [] }

    let propagateAction action = do 
          currentState <- Ref.read state
          currentGeometry <- Ref.read geometry
          result <- runTea currentState currentGeometry $ tea.handleAction action
          Ref.write result.state state
          Ref.write true dirty
          pure result.continuePropagation

    let loop = const do
          isDirty <- Ref.read dirty
          when isDirty do
            thisState <- Ref.read state

            let currentGeometry = provide tea.context $ tea.render thisState

            Ref.write currentGeometry geometry

            clearRect tea.context { x: 0.0, y: 0.0, width: 1000.0, height: 1000.0 }
            render tea.context currentGeometry 

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

            check geom = case Opt.toMaybe attribs.onClick of
                Just handler | pointInside event.position geom -> Just $ handler event
                _ -> Nothing
                where
                attribs = attributes geom 

            actions = dispatchEvent check currentGeometry

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

dispatchEvent :: forall a. (Geometry a -> Maybe a) -> Geometry a -> Array a
dispatchEvent check geometry = Array.catMaybes [Array.last nested, current]
    where
    current = check geometry
    nested = next >>= \child -> dispatchEvent check child
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