module Loglude.UntypedArray where

import Data.Array as Array
import Unsafe.Coerce (unsafeCoerce)

data UntypedArray

nil :: UntypedArray
nil = unsafeCoerce []

cons :: forall a. a -> UntypedArray -> UntypedArray
cons = unsafeCoerce (Array.cons)

infixr 6 cons as :