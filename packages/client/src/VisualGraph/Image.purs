module Lunarlog.VisualGraph.Image where

import Loglude

import Data.Array as Array
import Geometry as Geometry
import Geometry.Render.Image (ImageDataUrl, Promise, renderToImage)
import Geometry.Shapes.Flex (withMinimumSize)
import Graphics.Canvas (Context2D)
import Loglude.Editor.Settings (pinRadius)
import Lunarlog.Client.VisualGraph.Render (renderPatternLayout)
import Lunarlog.Core.NodeGraph as NodeGraph

type PatternSettings = 
    { name :: String
    , arguments :: Int
    }

-- | Render a pattern to a data url
renderPatternToImage :: PatternSettings -> Effect (Maybe (Promise ImageDataUrl))
renderPatternToImage { name, arguments } = renderToImage geometry
    where
    geometry :: Ask Context2D => _
    geometry = Geometry.aabbPadding
        { amount: Geometry.equalPadding pinRadius
        , target: withMinimumSize $ renderPatternLayout
            { lookupPattern: const $ Just $ NodeGraph.Unify $ NodeGraph.PinId 0
            , selectionIsNode: false
            , insideHead: false
            , hoveredPin: Nothing
            , pattern
            , offset: 0.0
            , nodeId: NodeGraph.NodeId 0
            }
        }

    pattern :: NodeGraph.Pattern
    pattern = 
        { name
        , arguments: Array.replicate arguments $ NodeGraph.NodeId 1 } 