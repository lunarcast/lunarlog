module Loglude.Editor.Actions where

import Loglude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Lens (traversed)
import Data.List (List)
import Data.List as List
import Data.Vec as Vec
import Data.ZipperArray as ZipperArray
import Effect.Class.Console (log)
import Geoemtry.Data.AABB as AABB
import Geometry (Vec2)
import Geometry.Base (ReporterOutput)
import Geometry.Tea (TeaM, absoluteBounds, awaitRerender, currentReport)
import Loglude.Data.Tree (TreeZipper)
import Loglude.Data.Tree as Tree
import Loglude.ReactiveRef as RR
import Loglude.Run.ExternalState (assign, modifying, use)
import Lunarlog.Client.VisualGraph.Types as VisualGraph
import Lunarlog.Core.NodeGraph (NodeId)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Editor.Types (EditorAction, EditorGeometryId(..), EditorState, HoverTarget(..), _atRuleNode, _atVisualRuleNode, _hovered, _mousePosition, _ruleBody, _ruleNode, _selection, freshNode, freshPin, idToHovered, selectionToId)
import Prelude (unless)

---------- Types
type ClientM = TeaM EditorState EditorGeometryId EditorAction

---------- Implementation
pointInsideReport :: Vec2 -> EditorGeometryId -> ClientM Boolean
pointInsideReport point reportId = absoluteBounds reportId <#> maybe false (AABB.pointInside point)

isHovered :: Vec2 -> HoverTarget -> ClientM Boolean
isHovered _ NothingHovered = pure false
isHovered point (HoveredPinDropZone id) = pointInsideReport point $ NestedPinDropZone id
isHovered _ _ = pure false -- Unimplemented

hovered :: forall id. Hashable id => HashSet id -> Vec2 -> ReporterOutput id -> Array id 
hovered except point output = case makeZipper output.idTree of
    Just zipper -> tailRec go (zipper /\ List.Nil)
    Nothing -> []
    where
    makeZipper = Tree.toZipper >>> map ZipperArray.goLast
    finish = Array.fromFoldable >>> Done

    go :: TreeZipper id /\ List id -> Step (TreeZipper id /\ List id) (Array id)
    go (zipper /\ stack) = do
        let { inner, children } = ZipperArray.current zipper
        case AABB.pointInside point <$> HashMap.lookup inner output.absoluteBounds of
            Just true | not $ HashSet.member inner except -> case makeZipper children of
                Just zipper -> Loop $ zipper /\ List.Cons inner stack
                Nothing -> finish $ List.Cons inner stack
            _ -> case ZipperArray.goPrev zipper of
                Just zipper -> Loop $ zipper /\ stack
                Nothing -> finish stack


updateHovered :: ClientM Unit
updateHovered = do
    mousePosition <- use _mousePosition
    previouslyHovered <- use _hovered
    isStillHovered <- isHovered mousePosition previouslyHovered
    unless isStillHovered do
        report <- currentReport
        selection <- use _selection
        let except = maybe HashSet.empty HashSet.singleton $ selectionToId selection
        let currentlyHovered = case Array.head $ hovered except mousePosition report of
              Just hovered -> idToHovered hovered
              Nothing -> NothingHovered
        liftEffect $ logPretty currentlyHovered
        assign _hovered currentlyHovered

-- | Respond to a user clicking on a nested node
selectNestedNode :: { parent :: NodeId, nodeId :: NodeId } -> ClientM Unit
selectNestedNode { parent, nodeId } = do
    newPin <- freshPin
    newPinNode <- freshNode
    absoluteBounds (NodeGeometry nodeId) >>= traverse_ \bounds -> do
        position <- liftEffect $ RR.writeable bounds.position

        -- Create replacement pin
        assign (_atRuleNode newPinNode) $ Just $ NodeGraph.Unify newPin

        -- Create visual node for grabbed pattern
        assign (_atVisualRuleNode nodeId) $ Just $ VisualGraph.PatternNode { position }

        -- Mark the grabbed pattern as top-level
        modifying _ruleBody $ flip Array.snoc nodeId

        -- Remove the grabbed pattern from the argument list of the parent
        modifying (_ruleNode parent <<< NodeGraph._patternNode <<< NodeGraph._patternArguments <<< traversed) 
            \id -> if id == nodeId then newPinNode else id

        -- Trigger rerender for the grabbed pattern to resize
        awaitRerender

        currentReport <#> _.idTree <#> showPretty >>= log

        -- Move the resized pattern to look "good" relative to the mouse
        absoluteBounds (NodeGeometry nodeId) >>= traverse_ \bounds' -> do
            mousePosition <- use _mousePosition
            let relativeMousePosition = mousePosition - bounds.position

            -- size' * mouse / size
            let fixedRelativeMousePosition = Vec.zipWith (/) (Vec.zipWith (*) bounds'.size relativeMousePosition) bounds.size
            let newPosition = mousePosition - fixedRelativeMousePosition

            liftEffect $ RR.write newPosition position