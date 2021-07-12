module Loglude.Cancelable where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber, launchAff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import FRP.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventListener, EventTarget)
import Web.Event.EventTarget as Web.Event

---------- Types
newtype Cancelable a = Cancelable (Effect (a /\ Effect Unit))

newtype NeedsCancelerDict a = NeedsCancelerDict (WillCancel => a)

class WillCancel where
    shouldICancel :: Fiber Unit

---------- Typeclass instances
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

---------- Internal
passCancelerDict :: forall a. NeedsCancelerDict a -> (Fiber Unit -> a)
passCancelerDict withDict shouldICancel = unsafeCoerce withDict { shouldICancel }

---------- Helpers
makeAff :: forall a. ((Either Aff.Error a -> Effect Unit) -> Cancelable Unit) -> Aff a
makeAff f = Aff.makeAff \continue ->  canceler (f continue) <#> (liftEffect >>> const >>> Aff.Canceler)

pull :: Stream.Discrete ~> Aff
pull stream = Aff.makeAff \continue -> do
    ref <- Ref.new (pure unit)
    canceler_ <- Stream.subscribe stream \value -> do
        continue $ Right value
        join $ Ref.read ref
    Ref.write canceler_ ref
    pure $ Aff.Canceler $ const $ liftEffect canceler_

toDictForm :: forall a. Cancelable a -> WillCancel => Effect a
toDictForm (Cancelable cancelable) = do
    value /\ cancelMe <- cancelable
    launchAff_ do
        joinFiber shouldICancel
        liftEffect cancelMe
    pure value

fromDictForm :: forall a. (WillCancel => a) -> Cancelable a
fromDictForm needsDict = Cancelable do
    cancelClock <- Stream.create
    fiber <- launchAff do
        pull cancelClock.event
    pure $ giveMeADict fiber /\ cancelClock.push unit
    where
    giveMeADict = passCancelerDict $ NeedsCancelerDict needsDict 

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