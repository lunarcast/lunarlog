-- | Abusing the purescript codegen to implicitly pass some parameter
module Loglude.Ask where

import Unsafe.Coerce (unsafeCoerce)

class Ask a where
    ask :: a

provide :: forall a ctx. ctx -> (Ask ctx => a) -> a
provide = unsafeCoerce \context f -> f { ask: context }