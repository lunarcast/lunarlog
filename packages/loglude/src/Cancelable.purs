module Loglude.Cancelable where

import Prelude

import Data.Bifunctor (lmap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FRP.Stream as Stream
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventListener, EventTarget)
import Web.Event.EventTarget as Web.Event

newtype Cancelable a = Cancelable (Effect (a /\ Effect Unit))

instance Functor Cancelable where
    map f (Cancelable e) = Cancelable $ e <#> lmap f

instance Apply Cancelable where
    apply (Cancelable f) (Cancelable a) = Cancelable do
        function /\ cancelFunction <- f
        argument /\ cancelArgument <- a
        pure $ Tuple (function argument) (cancelFunction *> cancelArgument)

instance Applicative Cancelable where
    pure a = Cancelable $ pure $ a /\ pure unit

instance Bind Cancelable where
    bind (Cancelable a) function = Cancelable do
        argument /\ cancelArgument <- a
        let Cancelable r = function argument
        result /\ cancelResult <- r
        pure $ Tuple result $ cancelArgument *> cancelResult

instance Monad Cancelable

instance MonadEffect Cancelable where
    liftEffect effect = Cancelable (effect <#> flip Tuple (pure unit))

fromEffect :: Effect (Effect Unit) -> Cancelable Unit
fromEffect eff = eff <#> (/\) unit # Cancelable

fromCanceler :: Effect Unit -> Cancelable Unit
fromCanceler eff = Cancelable $ pure $ unit /\ eff

canceler :: forall a. Cancelable a -> Effect (Effect Unit)
canceler (Cancelable inner) = inner <#> snd

perform :: forall a. Cancelable a -> Effect a
perform (Cancelable inner) = inner <#> fst

createStream :: forall a. ((a -> Effect Unit) -> Cancelable Unit) -> Stream.Discrete a
createStream f = Stream.makeEvent \emit -> f emit # canceler 

subscribe :: forall a. Stream.Discrete a -> (a -> Effect Unit) -> Cancelable Unit
subscribe stream = Stream.subscribe stream >>> fromEffect

addEventListener :: EventType -> EventListener -> Boolean -> EventTarget -> Cancelable Unit
addEventListener type_ listener capture target = Cancelable do
    Web.Event.addEventListener type_ listener capture target
    pure $ unit /\ Web.Event.removeEventListener type_ listener capture target