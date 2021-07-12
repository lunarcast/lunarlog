-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex 
    ( Arrangement(..)
    , Alignment(..)
    , LayoutChild(..)
    , FlexInputAttributes
    , PureFlexLayout
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

data LayoutChild a
    = NotLayout (Geometry a)
    | IsLayout (FlexLayout a)

type FlexInputAttributes :: (Type -> Type) -> Attributes
type FlexInputAttributes f a r = 
    ( children :: Array (LayoutChild a)
    , flexAxis :: Axis
    , stretchChildren :: f Boolean
    , enforceSize :: f Boolean
    , arrangeChildren :: f Arrangement
    , alignChildren :: f Alignment
    , wrap :: f (Geometry a -> Geometry a)
    , position :: f Vec2
    | r )

type PureFlexLayout a = 
    { fixSize :: Vec2 -> Geometry a
    , minimumSize :: Vec2
    , position :: Vec2
    }

type FlexLayout a = PureFlexLayout a

withDefaults :: forall a. Record (FlexInputAttributes Opt a ()) -> Record (FlexInputAttributes Id a ())
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

withFixedSize :: forall a. FlexLayout a -> Vec2 -> Geometry a
withFixedSize { fixSize } size = fixSize size 

withMinimumSize :: forall a. FlexLayout a -> Geometry a
withMinimumSize { fixSize, minimumSize } = fixSize minimumSize

wrapLayout :: forall a. Ask Context2D => (Geometry a -> Geometry a) -> FlexLayout a -> FlexLayout a
wrapLayout wrapper { minimumSize, position, fixSize } = { position, minimumSize: wrappedMinimumSize, fixSize: \size -> fixSize (size - deltaSize) # wrapper }
    where
    deltaSize :: Vec2
    deltaSize = wrappedMinimumSize - minimumSize 

    wrappedMinimumSize :: Vec2
    wrappedMinimumSize = wrapSize minimumSize

    wrapSize :: Vec2 -> Vec2
    wrapSize size = maybe zero _.size $ bounds $ wrapper $ rect { position: position, size }

-- | Create a flex layout
createFlexLayout :: forall given action. Ask Context2D => Closed.Coerce given (Record (FlexInputAttributes Opt action ())) => given -> FlexLayout action
createFlexLayout = Closed.coerce >>> withDefaults >>>_createLayout

-- | Internal version of createLayout with fully saturated inputs
_createLayout :: forall a. Ask Context2D => Record (FlexInputAttributes Id a ()) -> FlexLayout a
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

    stackFixed :: Vec2 -> Geometry a
    stackFixed fixedSize = group
        { children: processShapes $ Array.scanl scanner (startingOffset /\ None zero) childrenWithFixedSizes <#> snd
        , label: "Flex container"
        }
        where
        processShapes :: Array (Geometry a) -> Array (Geometry a)
        processShapes shapes | enforceSize = Array.cons invisibleAABB shapes
                             | otherwise = shapes

        invisibleAABB :: Geometry a
        invisibleAABB = Geometry.rect
            { position
            , size: fixedSize
            }

        childrenWithSizes :: Array (LayoutChild a /\ AABB)
        childrenWithSizes = Array.zip children childrenSizes

        childrenWithFixedSizes :: Array (Geometry a /\ AABB)
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

        scanner :: (Number /\ Geometry a) -> (Geometry a /\ AABB) -> (Number /\ Geometry a)
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