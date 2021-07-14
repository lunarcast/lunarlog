module Lunarlog.Core.NodeGraph where

import Loglude

import Data.Hashable (class Hashable)

newtype NodeId = NodeId Int
newtype PinId = PinId Int

data PatternArgument
    = Pin PinId
    | NestedPattern Pattern

type Pattern =
    { name :: String
    , id :: NodeId
    , arguments :: Array PatternArgument
    }

data Node
    = PatternNode Pattern
    | Unify PinId

newtype Rule = Rule
    { head :: NodeId
    , nodes :: HashMap NodeId Node
    , connections :: HashMap PinId PinId }

data LunarlogType
    = TypeVar String
    | TypeConstructor String
    | TypeApplication Type Type

type AdtBranch =
    { name :: String
    , arguments :: Array (String /\ LunarlogType)
    }

type AdtDeclaration =
    { name :: String
    , typeArgs :: Array String
    , branches :: Array AdtBranch }

data Declaration
    = RuleDeclaration (Array Rule)
    | AdtDeclaration AdtDeclaration

type Module =
    { name :: Array String
    , declarations :: Array Declaration }

---------- Lenses
_ruleNodes :: Lens' Rule (HashMap NodeId Node)
_ruleNodes = _Newtype <<< prop (Proxy :: _ "nodes")

---------- Typeclass instances
derive newtype instance Show PinId
derive newtype instance Show NodeId

derive instance Eq NodeId
derive instance Eq PinId

derive newtype instance Hashable NodeId
derive newtype instance Hashable PinId

derive instance Newtype Rule _