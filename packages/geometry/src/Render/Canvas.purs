module Geometry.Render.Canvas 
    ( render
    , multiStepRender
    ) where

import Loglude

import Data.Array as Array
import Data.Function (on)
import Data.List as List
import Data.Undefined.NoProblem (isUndefined)
import Data.ZipperArray as ZipperArray
import Geoemtry.Data.AABB (toCanvasRect)
import Geometry.Base (Geometry(..), GeometryAttributes, LayeredGeometry, MapActionF(..), MultiStepRenderer, ReporterOutput, mergeReporterOutputs, report)
import Geometry.Base as Geoemtry
import Geometry.Base as Geometry
import Geometry.Transform (TransformMatrix)
import Geometry.Vector (x, y)
import Graphics.Canvas (Context2D, arc, beginPath, fill, fillRect, fillText, lineTo, moveTo, stroke, strokeRect, strokeText, withContext)
import Prelude (compare)

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

-- | Unroll a multi step renderer
multiStepRender :: forall id action. Ask Context2D => Hashable id => MultiStepRenderer id action -> (Geometry id action /\ ReporterOutput id)
multiStepRender (initial /\ remaining) = do
    let initialReport = Geoemtry.report $ snd initial
    case ZipperArray.fromArray remaining of
        Nothing -> snd initial /\ initialReport
        Just remaining -> tailRec go (remaining /\ initialReport /\ List.singleton initial)
            # second (Array.fromFoldable >>> map snd >>> \children -> Geometry.group { children })
            # swap
    where
    go (remaining /\ previousReport /\ geometries) = do
        let currentGeometry = ZipperArray.current remaining previousReport
        let currentReport = mergeReporterOutputs previousReport (report $ snd currentGeometry)
        case ZipperArray.goNext remaining of
            Nothing -> Done (currentReport /\ insertGeometry currentGeometry)
            Just remaining -> Loop (remaining /\ currentReport /\ insertGeometry currentGeometry)
        where
        insertGeometry :: LayeredGeometry _ _ -> _
        insertGeometry currentGeometry = List.insertBy (on compare fst) currentGeometry geometries

render :: forall id action. Hashable id => Context2D -> Geometry id action -> Effect Unit
render context = case _ of
    Rect attributes -> withAttributes context attributes do
        let canvasRect = toCanvasRect attributes
        unless (isUndefined attributes.fill) do
            fillRect context canvasRect 
        unless (isUndefined attributes.stroke) do
            strokeRect context canvasRect
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
    Line attributes -> withAttributes context attributes do
        beginPath context
        moveTo context (x attributes.from) (y attributes.from)
        lineTo context (x attributes.to) (y attributes.to)
        endShape context attributes
    Transform attributes -> withAttributes context attributes do
        transform context attributes.transform
        render context attributes.target
    Group attributes ->  withAttributes context attributes do
        for_ attributes.children $ render context
    Text attributes -> withAttributes context attributes do
        let renderText renderer = renderer 
                context attributes.text 
                (x attributes.position)
                (y attributes.position)
        unless (isUndefined attributes.fill) $ renderText fillText
        unless (isUndefined attributes.stroke) $ renderText strokeText
    MapAction existential -> existential # runExists \(MapActionF { target }) -> render context target
    LockBounds { target } -> render context target
    Reporter { target, id, reportAbsoluteBounds, reportRelativeBounds } -> do
        render context target
    None _ -> pure unit

---------- Foreign imports
foreign import transform :: Context2D -> TransformMatrix -> Effect Unit
foreign import setAttributes :: 
    forall id action r. 
    Context2D -> 
    Record (GeometryAttributes id action r) -> 
    Effect Unit