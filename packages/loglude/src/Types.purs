-- | Trivial utility types
module Loglude.Types where

type Id :: forall k. k -> k
type Id a = a

type Const :: forall k1 k2. k1 -> k2 -> k1
type Const a b = a