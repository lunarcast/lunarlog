module Geometry.ExistsInside where


import Prim.Row as Row

data ExistsInside :: Row Type -> (Symbol -> Type -> Type) -> Type
data ExistsInside row f = ExistsInside (forall r. (forall key value rest. Row.Cons key value rest row => f key value -> r) -> r)

exists :: forall key value row f rest. Row.Cons key value rest row => f key value -> ExistsInside row f
exists inner = ExistsInside \f -> f inner

runExists :: forall row f r. ExistsInside row f -> (forall key value rest. Row.Cons key value rest row => f key value -> r) -> r
runExists (ExistsInside run) f = run f
