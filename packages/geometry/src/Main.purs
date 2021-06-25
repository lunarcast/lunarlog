module Geometry.Main where

import Loglude

import Data.Either (Either(..))
import Effect.Class.Console (log)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Stream as Stream
import Geometry.Types (Geometry, Vec2, circle, render, streamsToAttributes)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Loglude.Cancelable (perform)
import Loglude.Cancelable as Cancelable
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

onClick :: Stream.Discrete MouseEvent 
onClick = Cancelable.createStream \emit -> do
    liftEffect (window >>= document >>= body) >>= traverse_ \body_ -> do
        listener <- liftEffect $ eventListener 
            $ MouseEvent.fromEvent >>> traverse_ emit
        Cancelable.addEventListener EventTypes.click listener false (HtmlElement.toEventTarget body_)

eitherStream :: forall a b. Stream.Discrete a -> Stream.Discrete b -> Stream.Discrete (Either a b)
eitherStream first second = Cancelable.createStream \emit -> do
    Cancelable.subscribe first (Left >>> emit) 
    Cancelable.subscribe second (Right >>> emit) 
    
circleRadius :: Stream.Discrete Unit -> Stream.Discrete Int
circleRadius init = Stream.fold (\_ previous -> previous + 10) (eitherStream init onClick) 10

objToVec2 :: forall record. RecordLike record => record ( "0" :: Int, "1" :: Int ) -> Vec2
objToVec2 = unsafeCoerce

myCircle :: Stream.Discrete Unit -> Cancelable Geometry
myCircle init = do
    pos <- streamsToAttributes { "0": pure 100 :: Stream.Discrete _, "1": circleRadius init }
    pure $ circle { fill: "red" } (objToVec2 pos) 40

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= traverse_ \canvas -> do
        ctx <- getContext2D canvas
        init <- Stream.create
        geom <- perform $ myCircle init.event
        log $ unsafeCoerce geom
        init.push unit
        Stream.subscribe animationFrame \_ -> do
            render ctx geom

foreign import now :: Effect Number

{-

rect { onSomething: , otherAttribute }

Takes attributes and uses them
Takes events and registers them. Perhaps keep track of some event graph :hinking:

The geometry is a tree. We can put the events in a tree as well

-}