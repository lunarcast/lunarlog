module Data.Aged 
    ( dropDuplicates
    , dropDuplicatesOn
    ) where

import Prelude

import Data.Filterable (filterMap)
import Data.Lens (Fold, preview)
import Data.Maybe (Maybe(..), isNothing)
import Data.Maybe.First (First)
import FRP.Stream as Stream
import Unsafe.Reference (unsafeRefEq)


-- | Filter a stream to block all consecutive equal results of a getter
dropDuplicatesOn :: forall s t a b. Fold (First a) s t a b -> Stream.Discrete s -> Stream.Discrete s
dropDuplicatesOn getter = Stream.withLast >>> filterMap hasChanged
    where
    hasChanged { last, now } 
        | isNothing last || last `differentAges` now = Just now
        | otherwise = Nothing

    differentAges (Just a) b = case preview getter a, preview getter b of
        Just a, Just b -> not $ unsafeRefEq a b 
        Nothing, Nothing -> false
        _, _ -> true
    differentAges Nothing _ = true 


-- | Filter a stream to block all consecutive equal values
dropDuplicates :: forall a. Stream.Discrete a -> Stream.Discrete a
dropDuplicates = dropDuplicatesOn identity