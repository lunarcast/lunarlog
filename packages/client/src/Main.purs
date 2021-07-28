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
import Lunarlog.Editor.Types (ForeignAction(..), PatternShape)
import Lunarlog.VisualGraph.Image (renderPatternToImage)

type Constructors a=
    { createBranch :: Fn2 String Int a
    , editBranch :: Fn2 String Int a
    , createRule :: String -> a
    }

type ThumbailData =
    { name :: String
    , index :: Int
    , thumbail :: String
    }

type Result = 
    { thumbails :: Stream.Discrete ThumbailData }

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
    , createRule: CreateRule
    }

mainImpl :: ForeignInput ForeignAction -> Cancelable Result 
mainImpl input = do
    liftEffect (getCanvasElementById "canvas") >>= traverse_ \canvas ->  do
        ctx <- liftEffect $ getContext2D canvas
        liftEffect $ fixDpi ctx

        let zoom = 2.0

        provide ctx $ launchTea $ scene
            $ initialState input.initialState
    
    pure 
        { thumbails: empty
        }