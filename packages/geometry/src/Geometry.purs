module Geometry.Types where

import Loglude

import Data.Undefined.NoProblem.Closed as Closed
import Loglude.RecordLike as RecordLike

{- consumeGeometry ::  Stream.Discrete Geometry -> Cancelable Geometry
consumeGeometry stream = 
    do 
        let thisRef =  ref' none
        Cancelable.fromEffect $ Stream.subscribe stream $ write thisRef
        pure $ fromRef thisRef -}

data Geometry
-- data RefGeometry

type Vec2 = Vec D2 Int
type CanvasMouseEvent = { buttons :: Int }
type GeometryAttributes a = 
    { fill :: Opt String
    , onClick :: Opt CanvasMouseEvent -> a }

type GeometryConstructor a f 
    = forall given.
    Closed.Coerce given (GeometryAttributes a) => given -> f

type ForeignGeometryConstructor a result
    = GeometryAttributes a -> result

type GroupC = Array Geometry -> Geometry
type RectC = Vec2 -> Vec2 -> Geometry
type CircleC = Vec2 -> Int -> Geometry
-- type RefC = Geometry -> RefGeometry

---------- Constructors
none :: Geometry 
none = group' []

group :: GeometryConstructor GroupC 
group = RecordLike.coerce >>> _group

group' :: GroupC
group' = group {}

rect :: GeometryConstructor RectC
rect = RecordLike.coerce >>> _rect

rect' :: RectC
rect' = rect {}

circle :: GeometryConstructor CircleC
circle = RecordLike.coerce >>> _circle

circle' :: CircleC
circle' = circle {}

-- ref :: GeometryConstructor RefC
-- ref = RecordLike.coerce >>> _ref

-- ref' :: RefC
-- ref' = ref {}

---------- Foreign imports
foreign import _rect :: ForeignGeometryConstructor RectC
foreign import _circle :: ForeignGeometryConstructor CircleC
foreign import _group :: ForeignGeometryConstructor GroupC
-- foreign import _ref :: ForeignGeometryConstructor RefC

-- foreign import fromRef :: RefGeometry -> Geometry
-- foreign import write :: RefGeometry -> Geometry -> Effect Unit
-- foreign import render :: Context2D -> Geometry -> Effect Unit

---------- Streams to record
{- class StreamsToAttribs :: forall k. Row Type -> Row k -> Constraint
class StreamsToAttribs row target | row -> target where
    streamsToAttributes :: Record row -> Cancelable (MutableRecord target) 

instance (RowToList row rowList, StreamsToAttribsRL rowList row target) => StreamsToAttribs row target where
    streamsToAttributes = makeAttribsRL (Proxy :: _ rowList)

class StreamsToAttribsRL :: RowList Type -> Row Type -> Row Type -> Constraint
class StreamsToAttribsRL rowList row target | rowList row -> target where
    makeAttribsRL :: Proxy rowList -> Record row -> Cancelable (MutableRecord target)

instance 
    ( StreamsToAttribsRL tail row target
    , Row.Cons key (Stream.Discrete prop) remaining row
    , Row.Cons key prop left target
    , IsSymbol key
    ) => StreamsToAttribsRL (RowList.Cons key (Stream.Discrete prop) tail) row target where
    makeAttribsRL _ r = do
        let thisStream = Record.get key r
        result <- makeAttribsRL (Proxy :: _ tail) r
        Cancelable.fromEffect $ 
            Stream.subscribe thisStream \current -> MutableRecord.write key current result
        pure result
        where
        key :: Proxy key 
        key = Proxy
else instance Closed.Coerce {} (Record target) => StreamsToAttribsRL RowList.Nil row target where
    makeAttribsRL _ {} = pure $ MutableRecord.fromRecord (Closed.coerce {})  

---------- Streams to record 2
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
    , Row.Cons key (Stream.Discrete prop) remaining row
    , Row.Cons key prop left target
    , IsSymbol key
    ) => StreamsToAttribsRL (RowList.Cons key (Stream.Discrete prop) tail) row target where
    makeAttribsRL _ r = do
        result <- makeAttribsRL (Proxy :: _ tail) r
        let thisStream = Record.get key r
        let extended = MutableRecord.extend key (unsafeCoerce undefined) result
        Cancelable.fromEffect $ 
            Stream.subscribe thisStream \current -> do
                MutableRecord.write key current extended
        pure extended
        where
        key :: Proxy key 
        key = Proxy
else instance 
    (TypeEquals {| target } {}) => 
    StreamsToAttribsRL RowList.Nil row target where
    makeAttribsRL _ {} = pure $ MutableRecord.fromRecord $ (from {})
    -}
    