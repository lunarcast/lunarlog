module Data.Aged 
    ( Aged
    , aged
    , get
    , set
    , map
    , dropDuplicates
    , _aged
    ) where

import Prelude

import Data.Filterable (filterMap)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Stream as Stream

-- | Track when the last time a value has been modified was
newtype Aged a = Aged
    { entity :: a
    , age :: Number
    }

-- | Wrap a value in an aged context.
aged :: forall a. a -> Aged a
aged entity = Aged { entity, age: unsafePerformEffect now }

-- | Extract the value from withing an Aged type
get :: forall a. Aged a -> a
get (Aged { entity }) = entity

-- | Set the value contained by an Aged context.
-- | Also increases the age
set :: forall a. Aged a -> a -> Aged a
set = const aged 

-- | Lens for accessing an aged value. 
-- | Increases the age when modified
_aged :: forall a. Lens' (Aged a) a
_aged = lens get set

-- | I think the Functor instance isnt really lawful (map identity f != f), so this is the alternative
map :: forall a b. (a -> b) -> Aged a -> Aged b
map f (Aged { entity, age }) = Aged { entity: f entity, age  }

-- | Filter a stream to block all consecutive values with the same age
dropDuplicates :: forall a. Stream.Discrete (Aged a) -> Stream.Discrete a
dropDuplicates = Stream.withLast >>> filterMap hasChanged
    where
    hasChanged { last, now } 
        | isNothing last || last `differentAges` now = Just $ get now
        | otherwise = Nothing

    differentAges (Just (Aged a)) (Aged b) = a.age /= b.age
    differentAges Nothing _ = true 

---------- Foreign imports
foreign import now :: Effect Number