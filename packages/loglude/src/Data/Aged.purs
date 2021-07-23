module Data.Aged 
    ( dropDuplicates
    ) where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), isNothing)
import FRP.Stream as Stream
import Unsafe.Reference (unsafeRefEq)

-- | Filter a stream to block all consecutive values with the same age
dropDuplicates :: forall a. Stream.Discrete a -> Stream.Discrete a
dropDuplicates = Stream.withLast >>> filterMap hasChanged
    where
    hasChanged { last, now } 
        | isNothing last || last `differentAges` now = Just now
        | otherwise = Nothing

    differentAges (Just a) b = not $ unsafeRefEq a b
    differentAges Nothing _ = true 
