-- | Abusing the purescript codegen to implicitly pass some parameter
module Loglude.Ask where

import Unsafe.Coerce (unsafeCoerce)

class Ask a where
    context :: a

newtype AskDict e a = AskDict (Ask e => a)

ask :: forall e. Ask e => e
ask = inner
    where
    (AskDict inner) = unsafeCoerce _.context :: AskDict e e

provide :: forall a ctx. ctx -> (Ask ctx => a) -> a
provide = unsafeCoerce \context f -> f { context }