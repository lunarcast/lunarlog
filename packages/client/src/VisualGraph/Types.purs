module Lunarlog.Client.VisualGraph.Types where

import Loglude

import Geometry.Vector (Vec2)
import Lunarlog.Core.NodeGraph (NodeId, PinId)

---------- Types
type Pattern =
    { position :: Vec2 }

data Node
   = PatternNode Pattern
   | Unify { position :: Vec2 }

type Rule =
    { nodes :: HashMap NodeId Node
    , connections :: HashMap PinId PinId
    }

---------- Lenses
_ruleNodes :: Lens' Rule (HashMap NodeId Node)
_ruleNodes = prop (Proxy :: _ "nodes")

_patternNode :: Prism' Node Pattern
_patternNode = prism' PatternNode case _ of
    PatternNode pattern -> Just pattern
    _ -> Nothing