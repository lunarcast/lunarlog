-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex 
    ( Arrangement(..)
    , Alignment(..)
    , LayoutChild(..)
    , FlexInputAttributes
    , FlexLayout
    , withFixedSize
    , withMinimumSize
    , createFlexLayout
    , wrapLayout
    ) where

import Loglude

import Data.Array as Array
import Data.Foldable (maximum)
import Data.Undefined.NoProblem.Closed as Closed
import Geoemtry.Data.AABB (AABB)
import Geometry (Attributes, Context2D, Geometry(..), bounds, buildFromAxis, group, indexByAxis, other, rect, translate)
import Geometry as Geometry
import Geometry.Vector (Vec2, Axis, rmapAxis)
import Loglude as Opt

data Arrangement
    = ArrangeStart
    | ArrangeEnd
    | ArrangeCenter
    | SpaceEvenly
    | SpaceBetween

data Alignment
    = AlignStart
    | AlignMiddle
    | AlignEnd

data LayoutChild id action
    = NotLayout (Geometry id action)
    | IsLayout (FlexLayout id action)

type FlexInputAttributes :: (Type -> Type) -> Attributes
type FlexInputAttributes f id action r = 
    ( children :: Array (LayoutChild id action)
    , flexAxis :: Axis
    , stretchChildren :: f Boolean
    , enforceSize :: f Boolean
    , arrangeChildren :: f Arrangement
    , alignChildren :: f Alignment
    , wrap :: f (Geometry id action -> Geometry id action)
    , position :: f Vec2
    | r )

type FlexLayout id action = 
    { fixSize :: Vec2 -> Geometry id action
    , minimumSize :: Vec2
    , position :: Vec2
    }

withDefaults :: forall id action. Record (FlexInputAttributes Opt id action ()) -> Record (FlexInputAttributes Id id action ())
withDefaults attributes = 
    { stretchChildren: Opt.fromOpt true attributes.stretchChildren
    , arrangeChildren: Opt.fromOpt ArrangeStart attributes.arrangeChildren
    , alignChildren: Opt.fromOpt AlignStart attributes.alignChildren
    , enforceSize: Opt.fromOpt false attributes.enforceSize
    , position: Opt.fromOpt zero attributes.position
    , wrap: Opt.fromOpt identity attributes.wrap
    , children: attributes.children
    , flexAxis: attributes.flexAxis
    }

withFixedSize :: forall id action. FlexLayout id action -> Vec2 -> Geometry id action
withFixedSize { fixSize } size = fixSize size 

withMinimumSize :: forall id action. FlexLayout id action -> Geometry id action
withMinimumSize { fixSize, minimumSize } = fixSize minimumSize

wrapLayout :: forall id action. Ask Context2D => (Geometry id action -> Geometry id action) -> FlexLayout id action -> FlexLayout id action
wrapLayout wrapper { minimumSize, position, fixSize } = { position, minimumSize: wrappedMinimumSize, fixSize: \size -> fixSize (size - deltaSize) # wrapper }
    where
    deltaSize :: Vec2
    deltaSize = wrappedMinimumSize - minimumSize 

    wrappedMinimumSize :: Vec2
    wrappedMinimumSize = wrapSize minimumSize

    wrapSize :: Vec2 -> Vec2
    wrapSize size = maybe zero _.size $ bounds $ wrapper $ rect { position: position, size }

-- | Create a flex layout
createFlexLayout :: forall given id action. Ask Context2D => Closed.Coerce given (Record (FlexInputAttributes Opt id action ())) => given -> FlexLayout id action
createFlexLayout = Closed.coerce >>> withDefaults >>>_createLayout

-- | Internal version of createLayout with fully saturated inputs
_createLayout :: forall id action. Ask Context2D => Record (FlexInputAttributes Id id action ()) -> FlexLayout id action
_createLayout { flexAxis: axis, children, alignChildren, arrangeChildren, stretchChildren, position, wrap, enforceSize } = wrapLayout wrap
    { fixSize: \exact -> if Array.null children then None position else stackFixed exact
    , minimumSize
    , position 
    }
    where
    minimumSize :: Vec2
    minimumSize = buildFromAxis axis primarySize secondarySize

    childrenSizes :: Array AABB
    childrenSizes = children <#> childSize # Array.catMaybes 
        where
        childSize (NotLayout a) = bounds a
        childSize (IsLayout { position, minimumSize }) = Just { position, size: minimumSize }

    primarySize :: Number
    primarySize = foldr (+) 0.0 $ (_.size >>> indexByAxis axis) <$> childrenSizes
 
    secondarySize :: Number
    secondarySize = fromMaybe 0.0 $ maximum $ (indexByAxis (other axis) <<<_.size) <$> childrenSizes

    stackFixed :: Vec2 -> Geometry id action
    stackFixed fixedSize = group
        { children: processShapes $ Array.scanl scanner (startingOffset /\ None zero) childrenWithFixedSizes <#> snd
        , label: "Flex container"
        }
        where
        processShapes :: Array (Geometry id action) -> Array (Geometry id action)
        processShapes shapes | enforceSize = Array.cons invisibleAABB shapes
                             | otherwise = shapes

        invisibleAABB :: Geometry id action
        invisibleAABB = Geometry.rect
            { position
            , size: fixedSize
            }

        childrenWithSizes :: Array (LayoutChild id action /\ AABB)
        childrenWithSizes = Array.zip children childrenSizes

        childrenWithFixedSizes :: Array (Geometry id action /\ AABB)
        childrenWithFixedSizes = childrenWithSizes <#> uncurry case _, _ of
            NotLayout geometry, oldBounds -> geometry /\ oldBounds
            IsLayout { fixSize, minimumSize, position }, { size } -> do
                let fixedChild = fixSize $ rmapAxis axis childSecondarySize size
                fixedChild /\ { position, size }
            where
            childSecondarySize :: Number -> Number
            childSecondarySize minimumSize | stretchChildren = indexByAxis (other axis) fixedSize
                                           | otherwise = minimumSize

        totalSize :: Vec2
        totalSize = sum $ (_.size <<< snd) <$> childrenWithFixedSizes

        -- | How much empty space is going to be left on the primary axis
        emptyPrimarySpace :: Number
        emptyPrimarySpace = indexByAxis axis fixedSize - indexByAxis axis totalSize

        childrenCount :: Number
        childrenCount = toNumber $ Array.length children

        startingOffset :: Number
        startingOffset = case arrangeChildren of
            ArrangeEnd -> emptyPrimarySpace
            ArrangeCenter -> emptyPrimarySpace / 2.0
            SpaceBetween -> -emptyPrimarySpace / (childrenCount - 1.0)
            _ -> 0.0

        scanner :: (Number /\ Geometry id action) -> (Geometry id action /\ AABB) -> (Number /\ Geometry id action)
        scanner (offset /\ _) (geometry /\ fixedBounds) = newOffset /\ translate (screenOffset + position - fixedBounds.position) geometry
            where
            screenOffset :: Vec2
            screenOffset = buildFromAxis axis primary secondary
                where
                primary = offset + case arrangeChildren of
                    SpaceEvenly -> emptyPrimarySpace / (childrenCount + 1.0) 
                    SpaceBetween -> emptyPrimarySpace / (childrenCount - 1.0) 
                    _ -> 0.0 

                secondary = case alignChildren of
                    AlignStart -> 0.0
                    AlignEnd -> indexByAxis (other axis) fixedSize - indexByAxis (other axis) fixedBounds.size
                    AlignMiddle -> (indexByAxis (other axis) fixedSize - indexByAxis (other axis) fixedBounds.size) / 2.0

            newOffset :: Number
            newOffset = indexByAxis axis (fixedBounds.size + screenOffset)

---------- Typeclass instances
derive instance Generic Arrangement _

instance Show Arrangement where
    show = genericShow