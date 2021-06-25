module Loglude.RecordLike where

import Data.Undefined.NoProblem.Closed as Closed
import Loglude.MutableRecord (MutableRecord)
import Unsafe.Coerce (unsafeCoerce)

-- | Typeclass marking all the types which are implemnted as a record under the hood
class RecordLike :: (Row Type -> Type) -> Constraint
class RecordLike r

instance RecordLike Record
instance RecordLike MutableRecord

coerce :: 
    forall given expected record.
    RecordLike record =>
    Closed.Coerce (Record given) (Record expected) => 
    record given -> record expected
coerce g = unsafeCoerce g