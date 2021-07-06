module Lunarlog.Client.VisualGraph.Types where

import Loglude

import Geometry.Vector (Vec2)
import Lunarlog.Core.NodeGraph (NodeId, PinId)

type Pattern =
    { position :: Vec2 }

data Node
   = PatternNode Pattern
   | Unify { position :: Vec2 }

type Rule =
    { head :: Pattern
    , body :: HashMap NodeId Node
    , connections :: HashMap PinId PinId }