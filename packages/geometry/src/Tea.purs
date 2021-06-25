module Lunarlog.Tea where

import Loglude

import Control.Monad.State (StateT, execStateT)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem as Opt
import Data.Vec (vec2)
import Effect.Ref as Ref
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Types (CanvasMouseEvent, ClickCheck(..), Geometry, attributes, children, isClicked, none, render)
import Graphics.Canvas (Context2D, clearRect)
import Loglude.Cancelable as Cancelable
import Web.Event.Event (EventType)
import Web.Event.Internal.Types (Event)
import Web.HTML.Event.EventTypes as EventType
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

type SetupArgs :: Type -> Type -> Type
type SetupArgs s a = { propagateAction :: a -> Effect Unit }

type Tea s a =
    { initialState :: s
    , render :: s -> Geometry a
    , handleAction :: a -> StateT s Effect Unit
    , context :: Context2D
    , setup :: SetupArgs s a -> Cancelable Unit }

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

    let propagateAction action = do 
          currentState <- Ref.read state
          newState <- execStateT (tea.handleAction action) currentState
          Ref.write newState state
          Ref.write true dirty

    let loop = const do
          isDirty <- Ref.read dirty
          when isDirty do
            thisState <- Ref.read state

            let currentGeometry = tea.render thisState

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
                , position: vec2 
                    (MouseEvent.clientX ev) 
                    (MouseEvent.clientY ev) 
                }

            check geom = case Opt.toMaybe attribs.onClick of
                Just handler | isClicked checkMode event currentGeometry -> Just $ handler event
                _ -> Nothing
                where
                checkMode = Opt.fromOpt MouseInside attribs.clickChecker 
                attribs = attributes geom 

            actions = dispatchEvent check currentGeometry

        case Array.head actions of
            Just first -> propagateAction first
            Nothing -> pure unit

    tea.setup { propagateAction }
    where
    clicks :: Stream.Discrete MouseEvent
    clicks = eventStream (MouseEvent.fromEvent) EventType.click

    raf :: Stream.Discrete Unit
    raf = animationFrame


{-

We want event handlers to be able to:
- Continue propagation
- Access the current event
- Access the current geometry

-}

dispatchEvent :: forall a. (Geometry a -> Maybe a) -> Geometry a -> Array a
dispatchEvent check geometry = case check geometry of
    Just action -> [action]
    Nothing -> next >>= \child -> dispatchEvent check child
    where
    next = children geometry

eventStream :: forall e. (Event -> Maybe e) -> EventType -> Stream.Discrete e 
eventStream fromEvent eventType = Cancelable.createStream \emit -> do
    liftEffect (window >>= document >>= body) >>= traverse_ \body_ -> do
        listener <- liftEffect $ eventListener 
            $ fromEvent >>> traverse_ emit
        Cancelable.addEventListener eventType listener false (HtmlElement.toEventTarget body_)

