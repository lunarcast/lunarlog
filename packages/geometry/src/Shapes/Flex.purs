-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex 
    ( Arrangement(..)
    , Alignment(..)
    , LayoutChild(..)
    , FlexInputAttributes
    , OptionalFlexAttributes
    , FlexLayout
    , withFixedSize
    , withMinimumSize
    , createFlexLayout
    ) where

import Loglude

import Data.Array as Array
import Data.Foldable (maximum)
import Geometry (AllAttributes, Attributes, Geometry, Vec2, bounds, buildFromAxis, group, indexByAxis, other, rect, translate)
import Geometry as Geometry
import Geometry.Base (type (<+>), AABB, FullConstructor)
import Geometry.Shapes.None (none)
import Geometry.Vector (Axis, rmapAxis)
import Prelude (identity)
import Record.Unsafe.Union (unsafeUnion)

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
    | Offset Vec2 (LayoutChild a)

type FlexInputAttributes :: Attributes
type FlexInputAttributes r a = 
    ( children :: Array (LayoutChild a)
    , flexAxis :: Axis
    | r )

type OptionalFlexAttributes :: Attributes
type OptionalFlexAttributes r a =
    ( stretchChildren :: Boolean
    , enforceSize :: Boolean
    , arrangeChildren :: Arrangement
    , alignChildren :: Alignment
    , wrap :: Geometry a -> Geometry a 
    , position :: Vec2
    | r )

type FlexLayout a = 
    { fixSize :: Vec2 -> Geometry a
    , minimumSize :: Vec2
    , position :: Vec2
    }

defaults :: forall a. AllAttributes OptionalFlexAttributes a
defaults = 
    { stretchChildren: true
    , arrangeChildren: ArrangeStart
    , alignChildren: AlignStart
    , enforceSize: false
    , position: zero
    , wrap: identity
    }

withFixedSize :: forall a. FlexLayout a -> Vec2 -> Geometry a
withFixedSize { fixSize } size = fixSize size 

withMinimumSize :: forall a. FlexLayout a -> Geometry a
withMinimumSize { fixSize, minimumSize } = fixSize minimumSize

-- | Create a flex layout
createFlexLayout :: forall a. FullConstructor FlexLayout OptionalFlexAttributes FlexInputAttributes a
createFlexLayout attribs = unsafeUnion (unsafeCoerce attribs) defaults # _createLayout

-- | Internal version of createLayout with fully saturated inputs
_createLayout :: forall a. 
        AllAttributes (FlexInputAttributes <+> OptionalFlexAttributes) a -> 
        FlexLayout a
_createLayout { flexAxis: axis, children, alignChildren, arrangeChildren, stretchChildren, position, wrap, enforceSize } =
    { fixSize: \exact -> if Array.null children then none position else wrap $ stackFixed (exact - deltaSize)
    , minimumSize: wrappedMinimumSize
    , position: position
    }
    where
    deltaSize :: Vec2
    deltaSize = wrappedMinimumSize - minimumSize 

    wrappedMinimumSize :: Vec2
    wrappedMinimumSize = wrapSize minimumSize

    minimumSize :: Vec2
    minimumSize = buildFromAxis axis primarySize secondarySize

    wrapSize :: Vec2 -> Vec2
    wrapSize size = _.size $ bounds $ wrap $ rect { position: position, size }

    childSize = case _ of
        NotLayout a -> bounds a
        IsLayout { position, minimumSize } -> { position, size: minimumSize }
        Offset _ inner -> childSize inner

    childrenSizes :: Array AABB
    childrenSizes = children <#> childSize

    primarySize :: Number
    primarySize = foldr (+) 0.0 $ (_.size >>> indexByAxis axis) <$> childrenSizes
 
    secondarySize :: Number
    secondarySize = fromMaybe 0.0 $ maximum $ (indexByAxis (other axis) <<<_.size) <$> childrenSizes

    stackFixed :: Vec2 -> Geometry a
    stackFixed fixedSize = group
        { children: processShapes $ Array.scanl scanner (startingOffset /\ none zero) childrenWithFixedSizes <#> snd
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
        childrenWithFixedSizes = childrenWithSizes <#> uncurry fixSize
            where
            childSecondarySize :: Number -> Number
            childSecondarySize minimumSize | stretchChildren = indexByAxis (other axis) fixedSize
                                           | otherwise = minimumSize

            fixSize :: LayoutChild a -> AABB -> (Geometry a /\ AABB)
            fixSize (NotLayout geometry) oldBounds = geometry /\ oldBounds
            fixSize (IsLayout { fixSize, minimumSize, position }) { size } = do
                let fixedChild = fixSize $ rmapAxis axis childSecondarySize size
                fixedChild /\ { position, size }
            fixSize (Offset amount child) oldBounds = over ((_2 <<< _position)) mapPosition inner 
                where
                mapPosition = flip (-) amount
                inner = fixSize child oldBounds

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

---------- Lenses    
_position :: forall r. Lens' { position :: Vec2 | r } Vec2
_position = prop (Proxy :: _ "position")