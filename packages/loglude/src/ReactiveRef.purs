module Loglude.ReactiveRef 
    ( ReactiveRef(..)
    , WriteableRef
    , read
    , write
    , changes
    , writeable
    , fromStream ) where

import Prelude

import Control.Plus (empty)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Stream as Stream
import Loglude.Cancelable (Cancelable)
import Loglude.Cancelable as Cancelable
import Type.Proxy (Proxy(..))

newtype ReactiveRef a = ReactiveRef { read :: Effect a, changes :: Stream.Discrete a }

type WriteableRef a =
    { ref :: ReactiveRef a
    , write :: a -> Effect Unit
    }

read :: ReactiveRef ~> Effect
read = view _read

changes :: ReactiveRef ~> Stream.Discrete
changes = view _changes

fromStream :: forall a. a -> Stream.Discrete a -> Cancelable (ReactiveRef a)
fromStream initial changes = do
    ref <- liftEffect $ Ref.new initial
    Cancelable.subscribe changes $ flip Ref.write ref
    pure $ ReactiveRef { read: Ref.read ref, changes }

writeable :: forall a. a -> Cancelable (WriteableRef a)
writeable initial = do
    { event, push } <- liftEffect Stream.create
    readable <- fromStream initial event
    pure { write: push, ref: readable }


---------- Typeclass instances
derive instance Newtype (ReactiveRef a) _

derive instance Functor ReactiveRef

instance Apply ReactiveRef where
    apply (ReactiveRef f) (ReactiveRef a) = ReactiveRef
        { read: apply f.read a.read
        , changes: apply f.changes a.changes
        }

instance Applicative ReactiveRef where
    pure a = ReactiveRef { read: pure a, changes: empty }

write :: forall a. WriteableRef a -> a -> Effect Unit
write = view _write

---------- Lenses
_read :: forall a. Lens' (ReactiveRef a) (Effect a)
_read = _Newtype <<< prop (Proxy :: _ "read")

_changes :: forall a. Lens' (ReactiveRef a) (Stream.Discrete a)
_changes = _Newtype <<< prop (Proxy :: _ "changes")

_write :: forall a. Lens' (WriteableRef a) (a -> Effect Unit)
_write = prop (Proxy :: _ "write")