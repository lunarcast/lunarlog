module Geometry.Hovered where

import Loglude

import Data.Array as Array
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.List (List)
import Data.List as List
import Data.ZipperArray as ZipperArray
import Geoemtry.Data.AABB as AABB
import Geometry.Base (ReporterOutput, UnknownActionGeometryF(..))
import Geometry.Base as Geometry
import Geometry.Transform (multiplyVector)
import Geometry.Transform as Transform
import Geometry.Vector (Vec2)
import Graphics.Canvas (Context2D)
import Loglude.Data.Tree (TreeZipper)
import Loglude.Data.Tree as Tree

-- | Genereate a stack of whatever is being hovered over at the moment.
hovered :: forall id. Ask Context2D => Hashable id => HashSet id -> Vec2 -> ReporterOutput id -> Array id 
hovered except point output = case makeZipper output.idTree of
    Just zipper -> tailRec go (zipper /\ List.Nil)
    Nothing -> []
    where
    makeZipper = Tree.toZipper >>> map ZipperArray.goLast
    finish = Array.fromFoldable >>> Done

    go :: TreeZipper id /\ List id -> Step (TreeZipper id /\ List id) (Array id)
    go (zipper /\ stack) = do
        let { inner, children } = ZipperArray.current zipper
        case isHoveredOver inner of
            Just true | not $ HashSet.member inner except -> case makeZipper children of
                Just zipper -> Loop $ zipper /\ List.Cons inner stack
                Nothing -> finish $ List.Cons inner stack
            _ -> case ZipperArray.goPrev zipper of
                Just zipper -> Loop $ zipper /\ stack
                Nothing -> finish stack
        where
        isHoveredOver inner = case HashMap.lookup inner output.transforms, HashMap.lookup inner output.geometries of
            Just transform, Just geometry -> geometry # runExists 
                \(UnknownActionGeometryF geometry) -> Just $ Geometry.pointInside worldCoordinates geometry
                where
                worldCoordinates = multiplyVector inversed point
                -- TODO: look into caching this
                inversed = Transform.inverse transform
            _, _ -> AABB.pointInside point <$> HashMap.lookup inner output.absoluteBounds