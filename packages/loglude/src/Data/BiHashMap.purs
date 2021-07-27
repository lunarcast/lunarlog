module Loglude.Data.BiHashMap 
    ( BiHashMap
    , lookup
    , insert
    , delete
    , empty
    , half
    , toHashMap
    , insertMany
    , connections
    , _atBiHashMap
    ) where

import Prelude

import Data.Array as Array
import Data.Debug (class Debug, collection, constructor, debug)
import Data.Foldable (foldr)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Lens (Lens, lens)
import Data.Maybe (Maybe, fromMaybe, maybe')
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Safe.Coerce (coerce)

-- | A hashmap where `member a (lookup b h)` implies `member b (lookup a h)`
newtype BiHashMap key = BiHashMap (HashMap key (HashSet key))

lookup :: forall key. Hashable key => key -> BiHashMap key -> HashSet key
lookup key (BiHashMap hm) = fromMaybe HashSet.empty $ HashMap.lookup key hm

-- | Add a key to a BiHashMap.
-- | insert k v == insert v k
insert :: forall key. Hashable key => key -> key -> BiHashMap key -> BiHashMap key
insert from to = coerce (addToSet from to >>> addToSet to from)
    where
    addToSet from to = HashMap.insertWith HashSet.union from (HashSet.singleton to)

insertMany :: forall key. Hashable key => key -> HashSet key -> BiHashMap key -> BiHashMap key
insertMany from = flip $ foldr (insert from) 

delete :: forall key. Hashable key => key -> BiHashMap key -> BiHashMap key
delete key = coerce (HashMap.delete key >>> map (HashSet.delete key))

empty :: forall t. BiHashMap t
empty = BiHashMap HashMap.empty

toHashMap :: forall key. BiHashMap key -> HashMap key (HashSet key)
toHashMap (BiHashMap hm) = hm

connections :: forall key. Ord key => BiHashMap key -> Array (key /\ key)
connections = toHashMap >>> HashMap.toArrayBy connections >>> join >>> map orderEach >>> Array.nub
    where
    connections k v = map (k /\ _) $ HashSet.toArray v
    orderEach (a /\ b) = if a > b then a /\ b else b /\ a 

half :: forall key. Ord key => BiHashMap key -> Array key
half = connections >>> map fst

_atBiHashMap :: forall k. Hashable k => k -> Lens (BiHashMap k) (BiHashMap k) (HashSet k) (Maybe k)
_atBiHashMap k =
    lens (lookup k) \m ->
      maybe' (\_ -> delete k m) \v -> insert k v m

---------- Typeclass instances
instance (Debug d, Ord d, Hashable d) => Debug (BiHashMap d) where
    debug hm
        = connections hm
        # map (\(k /\ v)-> constructor "Pair" [debug k, debug v])
        # collection "BiHashMap"