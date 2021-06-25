module Geometry.Types.Old where

import Prelude

import Data.Symbol (class IsSymbol)
import Foreign (Foreign)
import Foreign.Object as Foreign
import Geometry.ExistsInside (ExistsInside, exists)
import Prim.Row as Row
import Type.Proxy (Proxy)

{- 
component = do
    smaller <- do 
        s <- withState
        c <- withInput
        input circle { pos: ..., radius: s, color: c }
    inner <- render $ translated [10, 10] smaller
    whenClicked inner \e -> ModifyState (+ 1)

translated = do
    onRender ...generate-some-hiccup 

vizualiseStuff = withInput \_ -> ...
-}

{-
A component has:
- A render method:
    - By default this calls the render of all the children
    - By default automatically gets called on state updates
    - This can be customized

    - The renderer generates some opaque hiccup geometry

- Some state (can be modified inside events)
, Some input (comes from the outside):
    - The state and input are used in the renderer and while generating the inputs for children. 

- Event focused: Takes events from the outside and focuses them for the inside:
    - Default handlers: Send to all children (last to first) until an event is raised
    - Custom handlers: can modify the payload, focus on specific components, raise events etc
- Event handlers: Handle events bubbled from the inner component:
    - Default handler: do nothing
    - Custom handler: modify state


At runtime:
    - The renderer is a simple function
    - The focusers and handlers are a monadic dsl which can:
        - modify the state
        - raise events
        - push events downwards
-}

type ComponentK = Type -> Type -> Row Type -> Row Type -> Type

data Component :: ComponentK
data Component state input eventsIn eventsOut

data Geometry
data EventHandler

newtype EventFocuserF state input eventsIn eventsOut key value = EventFocuserF 
    { name :: Proxy key
    , actions :: input -> Array (FocuserAction key value state input eventsIn eventsOut) }

type EventFocuser state input eventsIn eventsOut = ExistsInside eventsIn (EventFocuserF state input eventsIn eventsOut)

data ComponentSetting :: ComponentK 
data ComponentSetting state input eventsIn eventsOut
    = Renderer (state -> input -> Geometry)
    | Focuser (EventFocuser state input eventsIn eventsOut)
    | Handler EventHandler

data FocuserAction :: Symbol -> Type -> ComponentK 
data FocuserAction key value state input eventsIn eventsOut
    = MapPayload (value -> value)
    | StopPropagation
    | PropagateTo (Array (NodeWithIn key value))

data NodeId :: ComponentK 
data NodeId state input eventsIn eventsOut

data NodeWithIn :: Symbol -> Type -> Type
data NodeWithIn name payload

data ForeignNodeId

focus :: forall state input eventsIn eventsOut key value rest. IsSymbol key => Row.Cons key value rest eventsIn => Proxy key -> (input -> Array (FocuserAction key value state input eventsIn eventsOut)) -> ComponentSetting state input eventsIn eventsOut
focus name f = Focuser $ exists $ EventFocuserF { name, actions: f } 

---------- Coercions
type ForeignComponentSpec state input = 
    { render :: state -> input -> Geometry
    , focusers :: Foreign.Object (input -> Foreign -> { payload :: Foreign, to :: ForeignNodeId })
    , children :: Array ForeignNodeId
    }

