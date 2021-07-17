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
    , fromStream
    , mapChanges
    , mapUsingStream
    , dropDuplicates
    , pushAndWait
    ) where

import Prelude

import Control.Plus (empty)
import Data.Aged as Aged
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Stream as Stream
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

fromStream :: forall a. a -> Stream.Discrete a -> Effect (ReadableRef a)
fromStream initial changes = do
    ref <- Ref.new initial
    pure $ ReactiveRef { read: Ref.read ref, changes: changes # Stream.inspect (flip Ref.write ref) }

writeable :: forall a. a -> Effect (WriteableRef a)
writeable initial = do
    { event, push } <- Stream.create
    (ReactiveRef readable) <- fromStream initial event
    pure $ ReactiveRef { write: push, read: readable.read, changes: readable.changes }

readonly :: forall r. ReactiveRef r ~> ReadableRef
readonly (ReactiveRef { read, changes }) = ReactiveRef { read, changes }

-- | Modify the change detection stream.
mapChanges :: forall r a. (Stream.Discrete a -> Stream.Discrete a) -> ReactiveRef r a -> ReactiveRef r a
mapChanges = over _changes

-- | More or less map combined with mapChanges
mapUsingStream :: forall a b. (a -> b) -> (Stream.Discrete a -> Stream.Discrete b) -> ReadableRef a -> ReadableRef b
mapUsingStream mapRead mapChanges (ReactiveRef { read, changes }) = ReactiveRef
    { read: mapRead <$> read
    , changes: mapChanges changes
    }

-- | Reactive ref analogue of Aged.dropDuplicates
dropDuplicates :: forall a. ReadableRef (Aged.Aged a) -> ReadableRef a
dropDuplicates = mapUsingStream Aged.get Aged.dropDuplicates

-- | Write a value inside a ref and wait until the change has been propagated
pushAndWait :: forall a. a -> WriteableRef a -> Aff Unit
pushAndWait updated ref = do
    liftEffect $ write updated ref
    void $ Cancelable.pull $ changes ref

---------- Typeclass instances
coerceRef :: forall r. TypeEquals r () => ReactiveRef () ~> ReactiveRef r
coerceRef = unsafeCoerce

derive instance Newtype (ReactiveRef r a) _
instance TypeEquals r () => Functor (ReactiveRef r) where
    map f (ReactiveRef r) = coerceRef $ unsafePerformEffect do
        initial <- f <$> r.read
        fromStream initial (r.changes <#> f)

instance TypeEquals r () => Apply (ReactiveRef r) where
    apply (ReactiveRef f) (ReactiveRef a) = coerceRef $ unsafePerformEffect do
        let 
          changes = Stream.makeEvent \emit -> do
            latestFunction <- f.read >>= Ref.new
            latestArgument <- a.read >>= Ref.new
            cancelFunction <- Stream.subscribe f.changes \function -> do
                Ref.write function latestFunction
                Ref.read latestArgument >>= (function >>> emit)
            cancelArgument <- Stream.subscribe a.changes \argument -> do
                Ref.write argument latestArgument
                Ref.read latestFunction >>= \function -> emit (function argument)
            pure (cancelFunction *> cancelArgument)
        initial <- f.read <*> a.read
        fromStream initial changes

instance TypeEquals r () => Applicative (ReactiveRef r) where
    pure a = coerceRef $ ReactiveRef
        { read: pure a :: Effect _ 
        , changes: empty :: Stream.Discrete _
        }

instance TypeEquals r () => Bind (ReactiveRef r) where
    bind (ReactiveRef r) f = coerceRef $ unsafePerformEffect do
        let 
          changeStream :: Stream.Discrete _
          changeStream = Stream.makeEvent \emit -> do
            (cancelInner :: Ref.Ref (Effect Unit)) <- Ref.new (pure unit)
            cancelOuter <- Stream.subscribe (r.changes <#> f) \inner -> do
                Ref.read cancelInner # join
                read inner >>= emit
                canceler <- Stream.subscribe (changes inner) emit
                Ref.write canceler cancelInner
            pure do
                Ref.read cancelInner # join
                cancelOuter
        initial <- r.read <#> f >>= read 
        fromStream initial changeStream
    
instance TypeEquals r () => Monad (ReactiveRef r)

---------- Lenses
_read :: forall a r. Lens' (ReactiveRef r a) (Effect a)
_read = _Newtype <<< prop (Proxy :: _ "read")

_changes :: forall a r. Lens' (ReactiveRef r a) (Stream.Discrete a)
_changes = _Newtype <<< prop (Proxy :: _ "changes")

_write :: forall a. Lens' (WriteableRef a) (a -> Effect Unit)
_write = _Newtype <<< prop (Proxy :: _ "write")