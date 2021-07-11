-- | Flexbox inspired layouting system
module Geometry.Shapes.Flex 
    ( Arrangement(..)
    , Alignment(..)
    , LayoutChild(..)
    , FlexInputAttributes
    , OptionalFlexAttributes
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
import Data.Traversable (for)
import Geometry (AllAttributes, Attributes, Geometry, Vec2, bounds, buildFromAxis, group, indexByAxis, other, rect, translate)
import Geometry as Geometry
import Geometry.Base (type (<+>), AABB, FullConstructor)
import Geometry.Shapes.Effectful (effectful)
import Geometry.Shapes.None (none)
import Geometry.Vector (Axis, rmapAxis)
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

type PureFlexLayout a = 
    { fixSize :: Vec2 -> Geometry a
    , minimumSize :: Vec2
    , position :: Vec2
    }

type FlexLayout a = Effect (PureFlexLayout a)

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
withFixedSize layout size = effectful $ layout <#> \{ fixSize } -> fixSize size 

withMinimumSize :: forall a. FlexLayout a -> Geometry a
withMinimumSize layout = effectful $ layout <#> \{ fixSize, minimumSize } -> fixSize minimumSize

wrapLayout :: forall a. (Geometry a -> Geometry a) -> FlexLayout a -> FlexLayout a
wrapLayout wrapper layout = do
    { fixSize, minimumSize, position } <- layout
    
    wrappedMinimumSize <- map _.size $ bounds $ wrapper $ rect { position: position, size: minimumSize }
    let deltaSize = wrappedMinimumSize - minimumSize 

    pure { position, minimumSize: wrappedMinimumSize, fixSize: \size -> fixSize (size - deltaSize) # wrapper }

-- | Create a flex layout
createFlexLayout :: forall a. FullConstructor FlexLayout OptionalFlexAttributes FlexInputAttributes a
createFlexLayout attribs = unsafeUnion (unsafeCoerce attribs) defaults # _createLayout

-- | Internal version of createLayout with fully saturated inputs
_createLayout :: forall a. 
        AllAttributes (FlexInputAttributes <+> OptionalFlexAttributes) a -> 
        FlexLayout a
_createLayout { flexAxis: axis, children, alignChildren, arrangeChildren, stretchChildren, position, wrap, enforceSize } = do
    childrenSizes <- for children case _ of
        NotLayout a -> bounds a
        IsLayout layout -> layout <#> \layout -> { position: layout.position, size: layout.minimumSize }

    let primarySize = foldr (+) 0.0 $ (_.size >>> indexByAxis axis) <$> childrenSizes
    let secondarySize = fromMaybe 0.0 $ maximum $ (indexByAxis (other axis) <<<_.size) <$> childrenSizes

    let 
      stackFixed :: Vec2 -> Geometry a
      stackFixed fixedSize = effectful do 
        let
          invisibleAABB :: Geometry a
          invisibleAABB = Geometry.rect
            { position
            , size: fixedSize
            }

          processShapes :: Array (Geometry a) -> Array (Geometry a)
          processShapes shapes | enforceSize = Array.cons invisibleAABB shapes
                             | otherwise = shapes

          childrenWithSizes :: Array (LayoutChild a /\ AABB)
          childrenWithSizes = Array.zip children childrenSizes

        childrenWithFixedSizes <- do
            let childSecondarySize minimumSize 
                  | stretchChildren = indexByAxis (other axis) fixedSize
                  | otherwise = minimumSize

            for childrenWithSizes $ uncurry case _, _ of
                NotLayout geometry, oldBounds -> pure $ geometry /\ oldBounds
                IsLayout inner, { size } -> do
                    { position, minimumSize, fixSize } <- inner

                    let fixedChild = fixSize $ rmapAxis axis childSecondarySize size
                    pure $ fixedChild /\ { position, size }

        let
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

        pure $ group
          { children: processShapes $ Array.scanl scanner (startingOffset /\ none zero) childrenWithFixedSizes <#> snd
          , label: "Flex container"
          }

    wrapLayout wrap $ pure
        { fixSize: \exact -> if Array.null children then none position else stackFixed exact
        , minimumSize: buildFromAxis axis primarySize secondarySize
        , position 
        }

---------- Typeclass instances
derive instance Generic Arrangement _

instance Show Arrangement where
    show = genericShow