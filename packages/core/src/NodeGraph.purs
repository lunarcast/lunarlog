module Lunarlog.Core.NodeGraph where

import Loglude

import Loglude.Data.BiHashMap (BiHashMap)


newtype NodeId = NodeId Int
newtype PinId = PinId Int

type Pattern =
    { name :: String
    , arguments :: Array NodeId
    }

data Node
    = PatternNode Pattern
    | Unify PinId

newtype Rule = Rule
    { head :: NodeId
    , body :: Array NodeId
    , nodes :: HashMap NodeId Node
    , connections :: BiHashMap PinId
    }

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

type Module = HashMap (String /\ Int) Rule

---------- Lenses
_ruleNodes :: Lens' Rule (HashMap NodeId Node)
_ruleNodes = _Newtype <<< prop (Proxy :: _ "nodes")

_ruleBody :: Lens' Rule (Array NodeId)
_ruleBody = _Newtype <<< prop (Proxy :: _ "body")

_ruleHead :: Lens' Rule NodeId
_ruleHead = _Newtype <<< prop (Proxy :: _ "head")

_patternNode :: Prism' Node Pattern
_patternNode = prism' PatternNode case _ of
    PatternNode pattern -> Just pattern
    _ -> Nothing

_patternArguments :: Lens' Pattern (Array NodeId)
_patternArguments = prop (Proxy :: _ "arguments")

_ruleConnections :: Lens' Rule (BiHashMap PinId)
_ruleConnections = _Newtype <<< prop (Proxy :: _ "connections")

---------- Typeclass instances
derive newtype instance Show PinId
derive newtype instance Show NodeId

derive instance Generic Rule _
derive instance Generic Node _

derive newtype instance Debug PinId
derive newtype instance Debug NodeId

instance Debug Node where
    debug = genericDebug

instance Debug Rule where
    debug = genericDebug

derive instance Eq NodeId
derive instance Eq PinId

derive newtype instance Ord NodeId
derive newtype instance Ord PinId

derive newtype instance Hashable NodeId
derive newtype instance Hashable PinId

derive instance Newtype Rule _