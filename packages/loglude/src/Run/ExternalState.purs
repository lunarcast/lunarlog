module Loglude.Run.ExternalState 
    ( ExternalState
    , EXTERNAL_STATE
    , get
    , set
    , modify
    , modify_
    , gets
    , modifying
    , assign
    , use
    , runStateEffectfully
    , runStateUsingReactiveRef
    , runStateUsingRef
    , runStateUsingReactiveRefAsync
    ) where

import Loglude hiding (set)

import Data.Lens as Lens
import Effect.Ref as Ref
import Loglude.ReactiveRef as RR
import Run (liftAff)
import Run as Run

---------- Types
data ExternalState s a
    = Get (s -> a)
    | Put s a

type EXTERNAL_STATE s r = ( externalState :: ExternalState s | r )

---------- Typeclass isntances
derive instance Functor (ExternalState s)

---------- Constructors
get :: forall r s. Run (EXTERNAL_STATE s r) s
get = Run.lift _externalState $ Get identity

set :: forall r s. s -> Run (EXTERNAL_STATE s r) Unit
set s = Run.lift _externalState $ Put s unit

---------- Interpreters
runStateEffectfully :: forall s r. Effect s -> (s -> Effect Unit) -> Run (EXTERNAL_STATE s + EFFECT r) ~> Run (EFFECT r)
runStateEffectfully get set = Run.interpret (Run.on _externalState handle Run.send)
    where
    handle :: forall a. ExternalState s a -> Run (EFFECT r) a
    handle (Get continue) = liftEffect get <#> continue
    handle (Put state next) = liftEffect (set state) $> next

runStateUsingRef :: forall s r. Ref s -> Run (EXTERNAL_STATE s + EFFECT r) ~> Run (EFFECT r)
runStateUsingRef ref = runStateEffectfully (Ref.read ref) (flip Ref.write ref)

runStateUsingReactiveRef :: forall s r. WriteableRef s -> Run (EXTERNAL_STATE s + EFFECT r) ~> Run (EFFECT r)
runStateUsingReactiveRef ref = runStateEffectfully (RR.read ref) (flip RR.write ref)

runStateUsingReactiveRefAsync :: forall s r. WriteableRef s -> Run (EXTERNAL_STATE s + EFFECT + AFF r) ~> Run (EFFECT + AFF r)
runStateUsingReactiveRefAsync ref = Run.interpret (Run.on _externalState handle Run.send)
    where
    handle :: forall a. ExternalState s a -> Run (EFFECT + AFF r) a
    handle (Get continue) = liftEffect (RR.read ref) <#> continue
    handle (Put state next) = liftAff (RR.pushAndWait state ref) $> next

---------- Helpers
gets :: forall r a b. (a -> b) -> Run (EXTERNAL_STATE a r) b
gets f = get <#> f

modify :: forall r s. (s -> s) -> Run (EXTERNAL_STATE s r) s
modify f = do
    new <- gets f
    set new $> new

modify_ :: forall r s. (s -> s) -> Run (EXTERNAL_STATE s r) Unit
modify_ f = void $ modify f

---------- Lens helpers
modifying :: forall s a b r. Setter s s a b -> (a -> b) -> Run (EXTERNAL_STATE s r) Unit
modifying lens f = modify_ (Lens.over lens f)

assign :: forall s a b r. Setter s s a b -> b -> Run (EXTERNAL_STATE s r) Unit
assign lens v = modify_ (Lens.set lens v)

use :: forall s t a b r. Getter s t a b -> Run (EXTERNAL_STATE s r) a
use lens = gets $ Lens.view lens

---------- SProxies
_externalState :: Proxy "externalState"
_externalState = Proxy