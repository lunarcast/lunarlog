-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex where

import Loglude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Semigroup.Foldable (maximum)
import Data.Typelevel.Undefined (undefined)
import Foreign (unsafeFromForeign)
import Foreign.Object as Object
import Geometry (Attributes, Geometry, HiccupConfig, Vec2, AllAttributes, allAttributes, bounds, buildFromAxis, group, indexByAxis, none, other, translate)
import Geometry.Base (AABB)
import Geometry.Vector (Axis, rmapAxis)

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

data FlexAxis
    = FlexX
    | FlexY

type FlexInputAttributes :: Attributes
type FlexInputAttributes r a = 
    ( children :: NonEmptyArray (Geometry a)
    , position :: Vec2
    , flexAxis :: Axis
    , stretchChildren :: Boolean
    , arrangeChildren :: Arrangement
    , alignChildren :: Alignment
    | r )

type FlexComputedAttributes :: Attributes
type FlexComputedAttributes r a =
    ( fixSize :: Vec2 -> Geometry a
    , minimumSize :: Vec2 | r )

fixedSizeLayout :: forall a. AllAttributes FlexComputedAttributes a -> Vec2 -> Geometry a
fixedSizeLayout { fixSize } size = fixSize size 

withMinimumSize :: forall a. AllAttributes FlexComputedAttributes a -> Geometry a
withMinimumSize attributes = fixedSizeLayout attributes attributes.minimumSize

createLayout 
    :: forall a. 
        AllAttributes FlexInputAttributes a -> 
        AllAttributes FlexComputedAttributes a
createLayout { flexAxis: axis, children, alignChildren, arrangeChildren, stretchChildren, position } = 
    { fixSize: \exact -> stackFixed exact
    , minimumSize: buildFromAxis axis primarySize secondarySize
    }
    where
    childrenSizes :: NonEmptyArray AABB
    childrenSizes = bounds <$> children

    primarySize :: Number
    primarySize = NonEmptyArray.foldr1 (+) $ (_.size >>> indexByAxis axis) <$> childrenSizes

    secondarySize :: Number
    secondarySize = maximum $ (indexByAxis (other axis) <<<_.size) <$> childrenSizes

    stackFixed :: Vec2 -> Geometry a
    stackFixed fixedSize = group
        { children: NonEmptyArray.scanl scanner (startingOffset /\ none) childrenWithFixedSizes <#> snd # NonEmptyArray.toArray
        }
        where
        childrenWithSizes :: NonEmptyArray (Geometry a /\ AABB)
        childrenWithSizes = NonEmptyArray.zip children childrenSizes

        childrenWithFixedSizes :: NonEmptyArray (Geometry a /\ AABB)
        childrenWithFixedSizes = childrenWithSizes <#> fixSize
            where
            childSecondarySize :: Number -> Number
            childSecondarySize minimumSize | stretchChildren = indexByAxis (other axis) fixedSize
                                           | otherwise = minimumSize

            fixSize :: (Geometry a /\ AABB) -> (Geometry a /\ AABB)
            fixSize (child /\ { size }) = do
                let fixedChild = renderWithSize child
                        $ rmapAxis axis childSecondarySize size
                fixedChild /\ bounds fixedChild

        totalSize :: Vec2
        totalSize = sum $ (_.size <<< snd) <$> childrenWithFixedSizes

        -- | How much empty space is going to be left on the primary axis
        emptyPrimarySpace :: Number
        emptyPrimarySpace = indexByAxis axis fixedSize - indexByAxis axis totalSize

        childrenCount :: Number
        childrenCount = toNumber $ NonEmptyArray.length children

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

renderWithSize :: forall a. Geometry a -> Vec2 -> Geometry a 
renderWithSize geom finalSize = case Object.lookup "fixSize" $ allAttributes geom of
    Just foreignFixSize -> fixSize finalSize
        where
        fixSize :: Vec2 -> Geometry a 
        fixSize = unsafeFromForeign foreignFixSize
    Nothing -> geom

-- | Transforms a tuple x /\ y into a tuple 
-- | where the axis we are distributing the items on is first
orderByAxis :: forall a. FlexAxis -> a /\ a -> a /\ a
orderByAxis FlexX = identity
orderByAxis FlexY = swap

flexConfig :: HiccupConfig (AllAttributes FlexInputAttributes)
flexConfig = 
    { translate: undefined
    , toHiccup: unsafeCoerce (identity :: _ -> _)
    , aabbLike: opt \this -> pure { position: this.position, size: zero }  }


---------- Typeclass instances
derive instance Generic Arrangement _

instance Show Arrangement where
    show = genericShow
    
