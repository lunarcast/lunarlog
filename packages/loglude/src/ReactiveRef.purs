module Loglude.ReactiveRef 
    ( ReactiveRef(..)
    , ReadableRef
    , WriteableRef
    , read
    , write
    , modify
    , changes
    , writeable
    , readonly
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
import Effect.Unsafe (unsafePerformEffect)
import FRP.Stream as Stream
import Loglude.Cancelable (Cancelable)
import Loglude.Cancelable as Cancelable
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

readonly :: forall r. ReactiveRef r ~> ReadableRef
readonly (ReactiveRef { read, changes }) = ReactiveRef { read, changes }

---------- Typeclass instances
coerceRef :: forall r. TypeEquals r () => ReactiveRef () ~> ReactiveRef r
coerceRef = unsafeCoerce

derive instance Newtype (ReactiveRef r a) _

derive instance Functor (ReactiveRef r)

instance TypeEquals r () => Apply (ReactiveRef r) where
    apply (ReactiveRef f) (ReactiveRef a) = coerceRef $ ReactiveRef
        { read: f.read <*> a.read
        , changes: f.changes <*> a.changes
        }

instance TypeEquals r () => Applicative (ReactiveRef r) where
    pure a = coerceRef $ ReactiveRef
        { read: pure a :: Effect _ 
        , changes: empty :: Stream.Discrete _
        }

instance TypeEquals r () => Bind (ReactiveRef r) where
    bind = bindImpl

bindImpl :: forall a b r. TypeEquals r () => ReactiveRef r a -> (a -> ReactiveRef r b) -> ReactiveRef r b
bindImpl (ReactiveRef a) f = unsafeCoerce result
    where
    result :: ReactiveRef () b
    result = ReactiveRef
        { read: join (Ref.read theRef)
        , changes: resultStream
        }

    theRef = unsafePerformEffect do
        initial <- a.read <#> f <#> read
        ref <- Ref.new initial
        pure ref

    mapper ref = Ref.write (read ref) theRef $> ref

    resultStream :: Stream.Discrete b
    resultStream = Stream.do
        (ReactiveRef current) <- Stream.effectfulMap mapper $ map f $ a.changes
        current.changes
    
instance TypeEquals r () => Monad (ReactiveRef r)

---------- Lenses
_read :: forall a r. Lens' (ReactiveRef r a) (Effect a)
_read = _Newtype <<< prop (Proxy :: _ "read")

_changes :: forall a r. Lens' (ReactiveRef r a) (Stream.Discrete a)
_changes = _Newtype <<< prop (Proxy :: _ "changes")

_write :: forall a. Lens' (WriteableRef a) (a -> Effect Unit)
_write = _Newtype <<< prop (Proxy :: _ "write")