module Main 
    ( main
    , module Lunarlog.VisualGraph.Image
    ) where

import Loglude

import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import FRP.Stream as Stream
import Geometry (launchTea)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Editor (scene)
import Lunarlog.Editor.Types (ForeignAction(..), ForeignSubstitution, PatternShape)
import Lunarlog.Parser.Cst as Cst
import Lunarlog.VisualGraph.Image (renderPatternToImage)

type Constructors a =
    { createBranch :: Fn3 String Int PatternShape a
    , addNode :: Fn2 String Int a
    , editBranch :: Fn2 String Int a
    , deleteBranch :: Fn2 String Int a
    , togglePointerEvents :: Boolean -> a
    , evaluateQuery :: Cst.Pattern -> a
    }

type Result = 
    { queryResults :: Stream.Discrete (Nullable (Array ForeignSubstitution))
    }

type ForeignInput a =
    { actions :: Stream.Discrete a
    }

type Main = Constructors ~> ForeignInput -> Cancelable Result

main :: Main
main convert = mainImpl $ convert constructors

constructors :: Constructors ForeignAction
constructors =
    { createBranch: mkFn3 $ curry CreateBranch
    , editBranch: mkFn2 EditBranch
    , deleteBranch: mkFn2 DeleteBranch
    , addNode: mkFn2 AddNode
    , togglePointerEvents: TogglePointerEvents
    , evaluateQuery: EvaluateQuery
    }

mainImpl :: ForeignInput ForeignAction -> Cancelable Result 
mainImpl input = do
    queryResults <- liftEffect Stream.create
    liftEffect (getCanvasElementById "canvas") >>= traverse_ \canvas ->  do
        ctx <- liftEffect $ getContext2D canvas
        liftEffect $ fixDpi ctx

        let zoom = 2.0

        provide ctx $ launchTea $ scene
                { foreignActions: input.actions
                , emitQueryResult: queryResults.push
                }
    
    pure 
        { queryResults: queryResults.event <#> Nullable.toNullable
        }