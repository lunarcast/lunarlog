module Main 
    ( main
    , module Lunarlog.VisualGraph.Image
    ) where

import Loglude

import Data.Function.Uncurried (Fn2, mkFn2)
import Debug (spy)
import FRP.Stream as Stream
import Geometry (launchTea)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarlog.Canvas (fixDpi)
import Lunarlog.Editor (initialState, scene)
import Lunarlog.Editor.Types (ForeignAction(..), PatternShape, ThumnailData)
import Lunarlog.VisualGraph.Image (renderPatternToImage)
import Record as Record

type Constructors a=
    { createBranch :: Fn2 String Int a
    , addNode :: Fn2 String Int a
    , editBranch :: Fn2 String Int a
    , createRule :: String -> a
    }

type Result = 
    { thumnails :: Stream.Discrete ThumnailData }

type ForeignInput a =
    { actions :: Stream.Discrete a
    , initialState :: PatternShape
    }

type Main = (forall a. Constructors a -> ForeignInput a) -> Cancelable Result

main :: Main
main convert = mainImpl $ spy "input" $ convert constructors

constructors :: Constructors ForeignAction
constructors =
    { createBranch: mkFn2 CreateBranch
    , editBranch: mkFn2 EditBranch
    , addNode: mkFn2 AddNode
    , createRule: CreateRule
    }

mainImpl :: ForeignInput ForeignAction -> Cancelable Result 
mainImpl input = do
    thumnails <- liftEffect Stream.create
    liftEffect (getCanvasElementById "canvas") >>= traverse_ \canvas ->  do
        ctx <- liftEffect $ getContext2D canvas
        liftEffect $ fixDpi ctx

        let zoom = 2.0

        provide ctx $ launchTea $ scene
            $ Record.union (initialState input.initialState)
                { foreignActions: input.actions
                }
    
    pure 
        { thumnails: thumnails.event
        }