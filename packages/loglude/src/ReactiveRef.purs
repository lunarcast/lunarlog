module Loglude.ReactiveRef 
    ( ReactiveRef(..)
    , ReadableRef
    , WriteableRef
    , read
    , write
    , modify
    , changes
    , writeable
    , fromStream ) where

import Prelude

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

newtype ReactiveRef r a = ReactiveRef { read :: Effect a, changes :: Stream.Discrete a | r }

type ReadableRef = ReactiveRef ()
type WriteableRef a = ReactiveRef ( write :: a -> Effect Unit ) a

read :: forall r. ReactiveRef r ~> Effect
read = view _read

write :: forall a. a -> WriteableRef a -> Effect Unit
write = flip $ view _write

modify :: forall a. (a -> a) -> WriteableRef a -> Effect Unit
modify f ref = do
    value <- read ref
    write (f value) ref

changes :: forall r. ReactiveRef r ~> Stream.Discrete
changes = view _changes

fromStream :: forall a. a -> Stream.Discrete a -> Cancelable (ReadableRef a)
fromStream initial changes = do
    ref <- liftEffect $ Ref.new initial
    Cancelable.subscribe changes $ flip Ref.write ref
    pure $ ReactiveRef { read: Ref.read ref, changes }

writeable :: forall a. a -> Cancelable (WriteableRef a)
writeable initial = do
    { event, push } <- liftEffect Stream.create
    (ReactiveRef readable) <- fromStream initial event
    pure $ ReactiveRef { write: push, read: readable.read, changes: readable.changes }


---------- Typeclass instances
derive instance Newtype (ReactiveRef r a) _

derive instance Functor (ReactiveRef r)

---------- Lenses
_read :: forall a r. Lens' (ReactiveRef r a) (Effect a)
_read = _Newtype <<< prop (Proxy :: _ "read")

_changes :: forall a r. Lens' (ReactiveRef r a) (Stream.Discrete a)
_changes = _Newtype <<< prop (Proxy :: _ "changes")

_write :: forall a. Lens' (WriteableRef a) (a -> Effect Unit)
_write = _Newtype <<< prop (Proxy :: _ "write")