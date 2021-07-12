module FRP.Stream (module FRP.Event, Discrete, bind, discard, effectfulMap) where

import FRP.Event

import Effect (Effect)
import Effect.Ref as Ref
import Prelude (Unit, join, pure, unit, ($), (>>=))
import Prelude as Prelude

discard :: forall b. Discrete Unit -> (Unit -> Discrete b) -> Discrete b
discard = bind

bind :: forall a b. Discrete a -> (a -> Discrete b) -> Discrete b
bind a f = makeEvent \emit -> Prelude.do
    lastCanceler <- Ref.new (pure unit)
    canceler <- subscribe a \current -> Prelude.do
        canceler <- subscribe (f current) emit
        join $ Ref.read lastCanceler
        Ref.write canceler lastCanceler
    pure Prelude.do
        canceler
        join $ Ref.read lastCanceler

effectfulMap :: forall a b. (a -> Effect b) -> Discrete a -> Discrete b
effectfulMap f a = makeEvent \emit -> Prelude.do
    subscribe a \current -> f current >>= emit


---------- Types
type Discrete = Event