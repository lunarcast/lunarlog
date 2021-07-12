module Lunarlog.Client.VisualGraph.Types where

import Loglude

import Data.Aged (Aged)
import Geometry.Vector (Vec2)
import Lunarlog.Core.NodeGraph (NodeId, PinId)

type Pattern =
    { position :: WriteableRef (Aged Vec2) }

data Node
   = PatternNode Pattern
   | Unify { position :: Vec2 }

type Rule =
    { head :: Pattern
    , body :: HashMap NodeId Node
    , connections :: HashMap PinId PinId }