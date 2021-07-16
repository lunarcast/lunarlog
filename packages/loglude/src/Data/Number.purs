module Loglude.Data.Number where

import Prelude

-- Add 1 to a number
succ :: forall t. Semiring t => t -> t
succ = (+) one