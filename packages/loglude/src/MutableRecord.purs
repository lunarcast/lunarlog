module Loglude.MutableRecord (MutableRecord, read, write, fromRecord, extend) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Prim.Row as Row
import Type.Proxy (Proxy)

data MutableRecord :: forall k. Row k -> Type
data MutableRecord r

foreign import readImpl :: forall r. String -> MutableRecord r -> Effect Foreign
foreign import writeImpl :: forall r. String -> Foreign -> MutableRecord r -> Effect Unit
foreign import extendImpl :: forall r o. String -> Foreign -> MutableRecord r -> MutableRecord o
foreign import fromRecord :: forall r. Record r -> MutableRecord r

read :: 
    forall key value remaining all. 
    IsSymbol key =>
    Row.Cons key value remaining all => 
    Proxy key -> MutableRecord all -> Effect value
read key record = readImpl (reflectSymbol key) record <#> unsafeFromForeign 

write :: 
    forall key value remaining all. 
    IsSymbol key =>
    Row.Cons key value remaining all => 
    Proxy key -> value -> MutableRecord all -> Effect Unit
write key value record = writeImpl (reflectSymbol key) (unsafeToForeign value) record 

extend :: 
    forall key value remaining all. 
    IsSymbol key =>
    Row.Cons key value remaining all => 
    Proxy key -> value -> MutableRecord remaining -> MutableRecord all
extend key value record = extendImpl (reflectSymbol key) (unsafeToForeign value) record 