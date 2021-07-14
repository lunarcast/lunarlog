module Loglude.Data.Lens 
  ( _atHashMap
  , _atHashSet
  , _atHashSetRaw
  , _maybeUnitToBoolean
  ) where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as H
import Data.HashSet (HashSet)
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.Lens (Iso', Lens', iso, lens)
import Data.Maybe (Maybe(..), maybe')

---------- Missing instances
_atHashMap :: forall k v. Hashable k => k -> Lens' (HashMap k v) (Maybe v)
_atHashMap k =
    lens (H.lookup k) \m ->
      maybe' (\_ -> H.delete k m) \v -> H.insert k v m

-- | At implementation for hash sets
_atHashSetRaw :: forall v. Hashable v => v -> Lens' (HashSet v) (Maybe Unit)
_atHashSetRaw x = lens get (flip update)
    where
      get xs =
        if S.member x xs
           then Just unit
           else Nothing
      update Nothing = S.delete x
      update (Just _) = S.insert x

-- | Boolean implementation for AT on hash sets
_atHashSet :: forall v. Hashable v => v -> Lens' (HashSet v) Boolean
_atHashSet v = _atHashSetRaw v <<< _maybeUnitToBoolean

-- | Helper fro implementing atHashSet'
_maybeUnitToBoolean :: Iso' (Maybe Unit) Boolean
_maybeUnitToBoolean = iso to from
    where
    from true = Just unit
    from false = Nothing 
    
    to Nothing = false
    to _ = true 