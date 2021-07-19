module FRP.Stream (module FRP.Event, Discrete, Notifier, bind, discard, effectfulMap, inspect, notify, notifier) where

import Prelude hiding (discard, bind)

import Effect (Effect)
import FRP.Event (class Filterable, class IsEvent, Event, EventIO, count, create, filterMap, fix, fold, folded, gate, gateBy, keepLatest, makeEvent, mapAccum, sampleOn, sampleOn_, subscribe, withLast)

discard :: forall b. Discrete Unit -> (Unit -> Discrete b) -> Discrete b
discard = bind

bind :: forall a b. Discrete a -> (a -> Discrete b) -> Discrete b
bind a f = keepLatest (f <$> a)

effectfulMap :: forall a b. (a -> Effect b) -> Discrete a -> Discrete b
effectfulMap f a = makeEvent \emit -> Prelude.do
    subscribe a \current -> f current >>= emit

-- | Similar to subscribe, except the cancelation is bound to that of a newly created event
inspect :: forall a. (a -> Effect Unit) -> Event a -> Event a
inspect f = effectfulMap \v -> f v $> v

-- | Trigger a notification
notify :: Notifier -> Effect Unit
notify e = e.push unit

-- | Create a notifier
notifier :: Effect Notifier
notifier = create

---------- Types
type Discrete = Event

type Notifier = EventIO Unit