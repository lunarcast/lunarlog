module Loglude.Run.State (modifying, assign, use) where

import Prelude

import Data.Lens (Getter, Setter, over, set, view)
import Run (Run)
import Run.State (STATE, gets, modify)

modifying :: forall s a b r. Setter s s a b -> (a -> b) -> Run (STATE s r) Unit
modifying lens f = modify (over lens f)

assign :: forall s a b r. Setter s s a b -> b -> Run (STATE s r) Unit
assign lens v = modify (set lens v)

use :: forall s t a b r. Getter s t a b -> Run (STATE s r) a
use lens = gets $ view lens