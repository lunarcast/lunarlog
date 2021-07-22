module Loglude.Data.Tree where

import Prelude

import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Debug (class Debug, constructor, debug)
import Data.Generic.Rep (class Generic)

---------- Types
-- | Similar to Cofree, except not lazy, and can be empty
newtype Tree a = Tree (Array ({ inner :: a, children :: Tree a }))

---------- Helpers
-- | Construct a tree with no children
singleton :: forall a. a -> Tree a
singleton a = Tree [{ inner: a, children: empty }]

-- | Add a label onto a tree
annotate :: forall a. a -> Tree a -> Tree a
annotate inner children = Tree [{ inner, children }]

---------- Typeclass instances
instance Alt Tree where
    alt (Tree a) (Tree b) = Tree (a <|> b)

instance Plus Tree where
    empty = Tree []

instance Debug a => Debug (Tree a) where
    debug (Tree []) = constructor "Leaf" []
    debug (Tree [{ inner, children }]) = constructor "Annotate" [ debug inner, debug children ]
    debug (Tree many) = constructor "Node" $ map (pure >>> Tree >>> debug) many

derive instance Functor Tree
derive instance Generic (Tree a) _
