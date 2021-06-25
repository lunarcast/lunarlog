module Lunarlog.Tea where

import Loglude

import Control.Monad.State (StateT, execStateT)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (toMaybe)
import Data.Vec (vec2)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Types (CanvasMouseEvent, Geometry, attributes, children, none, pointInside, render)
import Graphics.Canvas (Context2D)
import Loglude.Cancelable as Cancelable
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.Event.EventTypes as EventType
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

type Tea s a =
    { initialState :: s
    , render :: s -> Geometry a
    , handleAction :: a -> StateT s Effect Unit
    , context :: Context2D }

data CanvasEvent
    = Click CanvasMouseEvent
    | MouseDown CanvasMouseEvent
    | MouseUp CanvasMouseEvent
    | MouseMove CanvasMouseEvent

launchTea :: forall state action. Tea state action -> Cancelable Unit
launchTea tea = do
    dirty <- liftEffect $ Ref.new true
    state <- liftEffect $ Ref.new tea.initialState
    geometry <- liftEffect $ Ref.new none

    let loop = const do
          isDirty <- Ref.read dirty
          when isDirty do
            thisState <- Ref.read state

            let currentGeometry = tea.render thisState

            log $ unsafeCoerce currentGeometry

            Ref.write currentGeometry geometry
            render tea.context currentGeometry 

            Ref.write false dirty

    Cancelable.subscribe raf loop
    Cancelable.subscribe clicks \ev -> do
        currentGeometry <- Ref.read geometry
        currentState <- Ref.read state

        let event :: CanvasMouseEvent
            event = 
                { buttons: MouseEvent.buttons ev
                , position: vec2 
                    (MouseEvent.clientX ev) 
                    (MouseEvent.clientY ev) 
                }

            actions = dispatchEvent currentGeometry event

        case Array.head actions of
            Just first -> do
                newState <- execStateT (tea.handleAction first) currentState
                Ref.write newState state
                Ref.write true dirty
            Nothing -> pure unit
    where
    clicks :: Stream.Discrete MouseEvent
    clicks = eventStream (MouseEvent.fromEvent) EventType.click

    raf :: Stream.Discrete Unit
    raf = animationFrame

dispatchEvent :: forall a. Geometry a -> CanvasMouseEvent -> Array a
dispatchEvent geometry event = case toMaybe onClick of
    Just handler | pointInside geometry event.position -> pure $ handler event
    _ -> next >>= \child -> dispatchEvent child event
    where
    { onClick } = attributes geometry
    next = children geometry

eventStream :: forall e. (Event -> Maybe e) -> EventType -> Stream.Discrete e 
eventStream fromEvent eventType = Cancelable.createStream \emit -> do
    liftEffect (window >>= document >>= body) >>= traverse_ \body_ -> do
        listener <- liftEffect $ eventListener 
            $ fromEvent >>> traverse_ emit
        Cancelable.addEventListener eventType listener false (HtmlElement.toEventTarget body_)