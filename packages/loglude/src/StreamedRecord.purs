module StreamedRecord where

import Loglude

import FRP.Stream as Stream
import Loglude.Cancelable as Cancelable
import Loglude.MutableRecord as MutableRecord
import Loglude.ReactiveRef as RR
import Prim.Row as Row
import Prim.RowList as RowList
import Record as Record
import Type.Equality (class TypeEquals, from)

class StreamsToAttribs :: forall k. Row Type -> Row k -> Constraint
class StreamsToAttribs row target | row -> target where
    streamsToAttributes :: Record row -> Cancelable (MutableRecord target) 

instance (RowToList row rowList, StreamsToAttribsRL rowList row target) => StreamsToAttribs row target where
    streamsToAttributes = makeAttribsRL (Proxy :: _ rowList)

class StreamsToAttribsRL :: RowList Type -> Row Type -> Row Type -> Constraint
class StreamsToAttribsRL rowList row target | rowList row -> target where
    makeAttribsRL :: Proxy rowList -> Record row -> Cancelable (MutableRecord target)

instance 
    ( StreamsToAttribsRL tail row left
    , Row.Cons key (RR.ReactiveRef prop) remaining row
    , Row.Cons key prop left target
    , IsSymbol key
    ) => StreamsToAttribsRL (RowList.Cons key (RR.ReactiveRef prop) tail) row target where
    makeAttribsRL _ r = do
        result <- makeAttribsRL (Proxy :: _ tail) r
        let thisStream = Record.get key r
        initial <- liftEffect $ RR.read thisStream
        let extended = MutableRecord.extend key initial result
        Cancelable.fromEffect $ 
            Stream.subscribe (RR.changes thisStream) \current -> do
                MutableRecord.write key current extended
        pure extended
        where
        key :: Proxy key 
        key = Proxy
else instance 
    (TypeEquals {| target } {}) => 
    StreamsToAttribsRL RowList.Nil row target where
    makeAttribsRL _ {} = pure $ MutableRecord.fromRecord $ (from {})