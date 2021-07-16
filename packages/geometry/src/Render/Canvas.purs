module Geometry.Render.Canvas (render) where

import Loglude

import Data.Undefined.NoProblem (isUndefined)
import Geoemtry.Data.AABB (toCanvasRect)
import Geometry.Base (Geometry(..), GeometryAttributes, MapActionF(..))
import Geometry.Transform (TransformMatrix)
import Geometry.Vector (x, y)
import Graphics.Canvas (Context2D, arc, beginPath, fill, fillRect, fillText, stroke, strokeRect, strokeText, withContext)

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

render :: forall id action. Context2D -> Geometry id action -> Effect Unit
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
    None _ -> pure unit

---------- Foreign imports
foreign import transform :: Context2D -> TransformMatrix -> Effect Unit
foreign import setAttributes :: 
    forall id action r. 
    Context2D -> 
    Record (GeometryAttributes id action r) -> 
    Effect Unit