-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex where

import Loglude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Semigroup.Foldable (maximum)
import Data.Tuple (fst, snd)
import Foreign (unsafeFromForeign)
import Foreign.Object as Object
import Geometry (AllAttributes, Attributes, Geometry, HiccupConfig, Vec2, allAttributes, bounds, group, none, translate)
import Geometry.Vector as Vec

data Arrangement
    = Start
    | End
    | Center
    | Evenly

data Alignment
    = Top
    | Middle
    | Bottom

data FlexAxis
    = FlexX
    | FlexY

type FlexInputAttributes :: Attributes
type FlexInputAttributes r a = 
    ( children :: NonEmptyArray (Geometry a)
    , position :: Vec2
    , flexAxis :: FlexAxis
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

createLayout 
    :: forall a. 
        AllAttributes FlexInputAttributes a -> 
        AllAttributes FlexComputedAttributes a
createLayout { flexAxis, children, stretchChildren, position } = 
    { fixSize: \exact -> stackFixed $ fixChildren $ orderByAxis flexAxis $ Vec.toTuple exact
    , minimumSize: Vec.fromTuple $ orderByAxis flexAxis $ primarySize /\ secondarySize
    }
    where
    childrenSizes :: NonEmptyArray (Pair Number)
    childrenSizes = orderByAxis flexAxis <$> Vec.toTuple <$> size <$> children

    primary /\ secondary = NonEmptyArray.unzip childrenSizes

    primarySize :: Number
    primarySize = NonEmptyArray.foldr1 (+) primary

    secondarySize :: Number
    secondarySize = maximum secondary 

    fixChild :: Pair Number -> Geometry a -> Pair Number -> (Pair Number /\ Geometry a)
    fixChild (primarySize /\ secondarySize) child childSize = 
        size /\ renderWithSize child orderedSize 
        where
        orderedSize :: Vec2
        orderedSize = Vec.fromTuple $ orderByAxis flexAxis size

        size :: Pair Number
        size = fst childSize /\ if stretchChildren then secondarySize else snd childSize

    fixChildren :: Pair Number -> NonEmptyArray (Pair Number /\ Geometry a)
    fixChildren sizes = zipped <#> uncurry (fixChild sizes)
        where
        zipped :: NonEmptyArray (Geometry a /\ Pair Number)
        zipped = NonEmptyArray.zip children childrenSizes

    stackFixed :: NonEmptyArray (Pair Number /\ Geometry a) -> Geometry a
    stackFixed children = group
        { children: NonEmptyArray.scanl scanner (0.0 /\ none) children <#> snd # NonEmptyArray.toArray
        }
        where
        scanner :: (Number /\ Geometry a) -> (Pair Number /\ Geometry a) -> (Number /\ Geometry a)
        scanner (offset /\ _) (size /\ geometry) = newOffset /\ translate (screenOffset + position) geometry
            where
            screenOffset :: Vec2
            screenOffset = Vec.fromTuple $ orderByAxis flexAxis $ (offset + fst childOffset) /\ snd childOffset

            childBounds = bounds geometry
            childOffset = -(orderByAxis flexAxis $ Vec.toTuple $ childBounds.position)
            newOffset = offset + fst size 

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
    { _type: "group"
    , toHiccup: unsafeCoerce (identity :: _ -> _)
    , aabbLike: opt \this -> pure { position: this.position, size: zero }  }

-- Assumes flexSize has the correct type ¯\_(ツ)_/¯
size :: forall a. Geometry a -> Vec2
size geom = case Object.lookup "minimumSize" $ allAttributes geom of
    Just size_ -> unsafeFromForeign size_
    Nothing -> (bounds geom).size