module Lunarlog.Core.NodeGraph where

import Loglude

newtype NodeId = NodeId Int
newtype PinId = PinId Int

data PatternArgument
    = Pin PinId
    | NestedPattern Pattern

type Pattern =
    { name :: String
    , arguments :: Array PatternArgument }

data Node
    = PatternNode Pattern
    | Unify PinId

newtype Rule = Rule
    { head :: Pattern
    , body :: HashMap NodeId Node
    , connections :: HashMap PinId PinId }

data LunarlogType
    = TypeVar String
    | TypeConstructor String
    | TypeApplication Type Type

type AdtBranch =
    { name :: String
    , arguments :: Array LunarlogType }

type AdtDeclaration =
    { name :: String
    , args :: Array String
    , branches :: Array AdtBranch }

data Declaration
    = RuleDeclaration (Array Rule)
    | AdtDeclaration AdtDeclaration

type Module =
    { name :: Array String
    , declarations :: Array Declaration }