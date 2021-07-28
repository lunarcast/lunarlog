module Loglude.Run.ExternalState 
    ( ExternalState
    , EXTERNAL_STATE
    , get
    , set
    , put
    , modify
    , modify_
    , gets
    , modifying
    , assign
    , use
    , preuse
    , runStateEffectfully
    , runStateUsingReactiveRef
    , runStateUsingRef
    , runStateUsingReactiveRefAsync
    , runFocused
    , runPure
    ) where

import Loglude hiding (set)

import Data.Lens (Fold)
import Data.Lens as Lens
import Data.Maybe.First (First)
import Effect.Ref as Ref
import Loglude.ReactiveRef as RR
import Run (liftAff)
import Run as Run
import Run.State as State

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

put :: forall r s. s -> Run (EXTERNAL_STATE s r) Unit
put = set 

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

-- | Run some state under the focus of a bigger state
runFocused :: forall s a r. Lens' s a -> Run (EXTERNAL_STATE a + EXTERNAL_STATE s r) ~> Run (EXTERNAL_STATE s r)
runFocused lens = Run.interpret (Run.on _externalState handle Run.send)
    where
    handle :: ExternalState a ~> Run (EXTERNAL_STATE s r)
    handle (Get continue) = get <#> (Lens.view lens >>> continue)
    handle (Put state next) = (modify $ Lens.set lens state) $> next

-- | Converts the state to internal state and then runs it purely
runPure :: forall r s a. s -> Run (EXTERNAL_STATE s + STATE s r) a -> Run r (s /\ a)
runPure initial= Run.interpret (Run.on _externalState handle Run.send) >>> State.runState initial
    where
    handle :: ExternalState s ~> Run (STATE s r)
    handle (Get continue) = State.get <#> continue
    handle (Put state next) = State.put state $> next

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

preuse :: forall s t a b r. Fold (First a) s t a b -> Run (EXTERNAL_STATE s r) (Maybe a)
preuse lens = gets $ Lens.preview lens

---------- SProxies
_externalState :: Proxy "externalState"
_externalState = Proxy