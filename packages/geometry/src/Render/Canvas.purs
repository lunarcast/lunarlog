module Geometry.Render.Canvas 
    ( ReporterOutput
    , render
    , emptyReporterOutput
    , mergeReporterOutputs
    ) where

import Loglude

import Data.HashMap as HashMap
import Data.Undefined.NoProblem (isUndefined)
import Geoemtry.Data.AABB (AABB, toCanvasRect)
import Geoemtry.Data.AABB as AABB
import Geometry.Base (Geometry(..), GeometryAttributes, MapActionF(..), bounds)
import Geometry.Transform (TransformMatrix, multiplyVector)
import Geometry.Vector (x, y)
import Graphics.Canvas (Context2D, arc, beginPath, fill, fillRect, fillText, stroke, strokeRect, strokeText, withContext)

---------- Types
type ReporterOutput id =
    { absoluteBounds :: HashMap id AABB
    , relativeBounds :: HashMap id AABB
    }

---------- Constants
emptyReporterOutput :: forall id. ReporterOutput id
emptyReporterOutput =
    { absoluteBounds: HashMap.empty
    , relativeBounds: HashMap.empty
    }

---------- Helpers
mergeReporterOutputs :: forall id. Hashable id => ReporterOutput id -> ReporterOutput id -> ReporterOutput id
mergeReporterOutputs a b =
    { absoluteBounds: a.absoluteBounds `HashMap.union` b.absoluteBounds
    , relativeBounds: a.relativeBounds `HashMap.union` b.relativeBounds
    }

---------- Implementation
endShape :: forall id action r. Context2D -> Record (GeometryAttributes id action r) -> Effect Unit
endShape context attributes = do
    -- TODO: clipping
    unless (attributes.fill == opt "none" || isUndefined attributes.fill) $ fill context
    unless (attributes.stroke == opt "none" || isUndefined attributes.stroke) $ stroke context

withAttributes :: 
    forall id action r.
    Context2D ->
    Record (GeometryAttributes id action r) -> 
    Effect ~> Effect
withAttributes context attributes continue = withContext context do
    setAttributes context attributes
    continue

render :: forall id action. Hashable id => Context2D -> Geometry id action -> Effect (ReporterOutput id)
render context = case _ of
    Rect attributes -> withAttributes context attributes do
        let canvasRect = toCanvasRect attributes
        unless (isUndefined attributes.fill) do
            fillRect context canvasRect 
        unless (isUndefined attributes.stroke) do
            strokeRect context canvasRect
        pure emptyReporterOutput
    Circle attributes -> withAttributes context attributes do
        beginPath context
        arc context 
            { x: x attributes.position
            , y: y attributes.position
            , radius: attributes.radius
            , start: 0.0
            , end: tau
            }
        endShape context attributes
        pure emptyReporterOutput
    Transform attributes -> withAttributes context attributes do
        transform context attributes.transform
        output <- render context attributes.target
        pure
            { absoluteBounds: output.absoluteBounds # HashMap.mapMaybe (AABB.points >>> map (multiplyVector attributes.transform) >>> AABB.fromPoints)
            , relativeBounds: output.relativeBounds
            }
    Group attributes ->  withAttributes context attributes ado
        outputs <- for attributes.children $ render context
        in foldr mergeReporterOutputs emptyReporterOutput outputs
    Text attributes -> withAttributes context attributes do
        let renderText renderer = renderer 
                context attributes.text 
                (x attributes.position)
                (y attributes.position)
        unless (isUndefined attributes.fill) $ renderText fillText
        unless (isUndefined attributes.stroke) $ renderText strokeText
        pure emptyReporterOutput
    MapAction existential -> existential # runExists \(MapActionF { target }) -> render context target
    Reporter { target, id, reportAbsoluteBounds, reportRelativeBounds } -> ado
        output <- render context target
        in case provide context $ bounds target of
            Nothing -> output
            Just bounds ->
                { absoluteBounds: 
                    output.absoluteBounds # applyWhen reportAbsoluteBounds (HashMap.insert id bounds)
                , relativeBounds:
                    output.relativeBounds # applyWhen reportRelativeBounds (HashMap.insert id bounds)
                }
        where
        applyWhen condition f input | condition = f input
                                    | otherwise = input
    None _ -> pure emptyReporterOutput

---------- Foreign imports
foreign import transform :: Context2D -> TransformMatrix -> Effect Unit
foreign import setAttributes :: 
    forall id action r. 
    Context2D -> 
    Record (GeometryAttributes id action r) -> 
    Effect Unit