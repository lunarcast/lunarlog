module Loglude.Data.Tree where

import Prelude

import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Debug (class Debug, constructor, debug)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZipperArray

---------- Types
-- | A single tree node contains a label and a tree of children
type TreeNode a = { inner :: a, children :: Tree a }

-- | Similar to Cofree, except not lazy, and can be empty
newtype Tree a = Tree (Array (TreeNode a))

type TreeZipper a = ZipperArray (TreeNode a)

---------- Helpers
-- | Construct a tree with no children
singleton :: forall a. a -> Tree a
singleton a = Tree [{ inner: a, children: empty }]

-- | Add a label onto a tree
annotate :: forall a. a -> Tree a -> Tree a
annotate inner children = Tree [{ inner, children }]

-- | Create a zipper array out of the nodes at the top-level of a tree
toZipper :: forall a. Tree a -> Maybe (TreeZipper a)
toZipper (Tree inner) = ZipperArray.fromArray inner

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
