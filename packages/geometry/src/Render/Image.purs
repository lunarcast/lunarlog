module Geometry.Render.Image where

import Loglude

import Geometry (Geometry, Vec2, bounds, x, y)
import Geometry.Render.Canvas (render)
import Graphics.Canvas (CanvasElement, Context2D, getContext2D, setCanvasHeight, setCanvasWidth)

data ImageDataUrl
data Promise (a :: Type)

-- | Render a geometry to a data url
renderToImage :: forall id action. Hashable id => (Ask Context2D => Geometry id action) -> Effect (Maybe (Promise ImageDataUrl))
renderToImage contextToGeometry = do
    canvas <- createOffscreenCanvas $ vec2 1000.0 1000.0
    context <- getContext2D canvas
    let geometry = provide context contextToGeometry
    case provide context $ bounds geometry of
        Just bounds -> do
            setCanvasWidth canvas (x bounds.size)
            setCanvasHeight canvas (y bounds.size)
            render context geometry
            Just <$> toImageData canvas
        Nothing -> pure Nothing

---------- Foreign imports
foreign import createOffscreenCanvas :: Vec2 -> Effect CanvasElement
foreign import toImageData :: CanvasElement -> Effect (Promise ImageDataUrl)