module Lunarlog.Expression where

import Loglude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.Lens (foldMapOf, iso)
import Data.List as List
import Data.Nullable as Nullable
import Data.String (joinWith)
import Data.Variant as Variant
import Loglude.Data.BiHashMap as BiHashMap
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Parser.Cst (withoutSpan)
import Lunarlog.Parser.Cst as Cst
import Prelude (class Show)
import Run.State (evalState, gets, modify)
import Run.Supply (generate, localSupply)

---------- Types
type Constructor a = { name :: String, arguments :: Array (Expression a) }

data Expression a
    = Var a
    | Constructor (Constructor a)

-- | A rule expresses the relation "in order to prove A we must prove B, C... first"
type Rule a =
    { head :: Constructor a
    , body :: Array (Constructor a)
    }

-- | Alias for the var cache used in insantiation
type INSTANTIATE key rest = STATE (HashMap key String) rest

type InstantiateM supplied key rest = Run (INSTANTIATE key + SUPPLY supplied rest)

---------- Conversion from CST
-- | Convert a Cst pattern into an expression
constructorFromCst :: Cst.Pattern -> Constructor String
constructorFromCst pattern@(Cst.Pattern { arguments, name }) =
    { arguments: fromCst <$> arguments
    , name: withoutSpan name }


-- | Convert a Cst term into an expression
fromCst :: Cst.Term -> Expression String
fromCst (Cst.Term t) = t # Variant.match
    { var: \s -> Var (withoutSpan s)
    , pattern: \c -> Constructor $ constructorFromCst c
    , natural: fromNatural
    , list: fromList
    }
    where
    fromList :: Cst.List -> Expression String
    fromList { elements, tail } = go (List.fromFoldable elements) 
        where
        go List.Nil = maybe nil fromCst $ Nullable.toMaybe tail
        go (List.Cons head rest) = cons (fromCst head) $ go rest

        nil :: Expression String
        nil = Constructor { name: "Nil", arguments: [] }

        cons :: forall a. Expression a -> Expression a -> Expression a
        cons head rest = Constructor { name: "Cons", arguments: [head, rest] }

    fromNatural :: Cst.WithSpan Int -> Expression String
    fromNatural (Cst.WithSpan { value }) = go value
        where
        go a | a > 0 = succ $ go (a - 1)
             | otherwise = zero

        zero :: forall a. Expression a
        zero = Constructor { name: "Z", arguments: [] }

        succ :: forall a. Expression a -> Expression a
        succ previous = Constructor { name: "S", arguments: [previous]  }

---------- Conversion from NodeGraphs
-- | Creates a rule out of its visual version.
-- | When 2 pins are connected, the one with the lower id is used as the variable name.
fromNodeGraph :: NodeGraph.Rule -> Maybe (Rule NodeGraph.PinId)
fromNodeGraph (NodeGraph.Rule { head, nodes, body, connections }) = ado
    head <- makeConstructor head
    body <- for (Array.delete head body) makeConstructor
    in { head, body }
    where
    makeConstructor :: NodeGraph.NodeId -> Maybe (Constructor NodeGraph.PinId)
    makeConstructor nodeId = case HashMap.lookup nodeId nodes of
        Just (NodeGraph.PatternNode { name, arguments }) -> ado
            arguments <- for arguments makeExpression
            in { name, arguments }
        _ -> Nothing

    makeExpression :: NodeGraph.NodeId -> Maybe (Expression NodeGraph.PinId)
    makeExpression nodeId = case HashMap.lookup nodeId nodes of
        Just (NodeGraph.Unify pinId) -> Just $ Var $ resolvePin pinId
        Just (NodeGraph.PatternNode { name, arguments }) -> ado
            arguments <- for arguments makeExpression
            in Constructor { name, arguments }
        Nothing -> Nothing

    resolvePin = resolvePinImpl HashSet.empty
    resolvePinImpl visited id = foldr (resolvePinImpl visited' >>> min) id unvisited
        where
        visited' = visited <> unvisited
        unvisited = BiHashMap.lookup id connections `HashSet.difference` visited

---------- Instantiation
-- | Helper to run the instantiation process
runInstantiation :: forall r key. Run (INSTANTIATE key r) ~> Run r
runInstantiation = evalState HashMap.empty

instantiate :: forall r key. Hashable key => Expression key ->  InstantiateM Int key r (Expression String)
instantiate = go >>> localSupply (show >>> (<>) "#")
    where
    go :: forall r'. Expression key -> InstantiateM String key r' (Expression String)
    go = traverse \var -> gets (HashMap.lookup var)
        >>= case _ of
            Just name -> pure name
            Nothing -> do
                name <- generate
                modify (HashMap.insert var name)
                pure name

instantiateConstructor :: forall key r. Hashable key => Constructor key -> InstantiateM Int key r (Constructor String)
instantiateConstructor = traverseOf _constructorArgument instantiate

-- | Get all free vars inside an expression
freeVars :: forall a. Hashable a => Expression a -> HashSet a
freeVars = foldMap HashSet.singleton

freeVarsConstructor :: forall a. Hashable a => Constructor a -> HashSet a
freeVarsConstructor = foldMapOf _constructorArgument freeVars

instantiateRule :: forall r. NodeGraph.Rule -> InstantiateM Int NodeGraph.PinId r (Maybe (Rule String))
instantiateRule = fromNodeGraph >>> traverseOf (_Just <<< _ruleExpression) instantiateConstructor

---------- Lenses
_constructorArguments :: forall a b. Lens (Constructor a) (Constructor b) (Array (Expression a)) (Array (Expression b))
_constructorArguments = prop (Proxy :: _ "arguments")

_constructorArgument :: forall a b. Traversal (Constructor a) (Constructor b) (Expression a) (Expression b)
_constructorArgument = _constructorArguments <<< traversed

_ruleExpression :: forall a b. Traversal (Rule a) (Rule b) (Constructor a) (Constructor b)
_ruleExpression = _ruleToNonEmptyArray <<< traversed
    where
    _ruleToNonEmptyArray = iso to from
    from = (NonEmptyArray.head &&& NonEmptyArray.tail) >>> uncurry { head: _, body: _ }
    to { head, body } = NonEmptyArray.cons' head body

---------- Typeclass instances
derive instance Functor Expression
derive instance Generic (Expression a) _

instance Debug a => Debug (Expression a) where
    debug a = genericDebug a

instance Foldable Expression where
    foldMap f (Var v) = f v
    foldMap f (Constructor { name, arguments }) 
        = arguments
        # foldMap (foldMap f)

    foldr a = foldrDefault a
    foldl a = foldlDefault a

instance Show (Expression String) where
    show (Var name) = name
    show constructor@(Constructor _) | Just natural <- extractNatural constructor = show natural
    show (Constructor { name, arguments }) = joinWith " "
        $ Array.cons name $ printArgument <$> arguments
        where
        printArgument constructor@(Constructor _) | Just natural <- extractNatural constructor = show natural 
        printArgument argument@(Constructor { arguments }) | not (Array.null arguments) = "(" <> show argument <> ")"
        printArgument other = show other

extractNatural :: forall a. Expression a -> Maybe Int
extractNatural (Constructor { arguments: [previous], name: "S" }) = ((+) 1) <$> extractNatural previous
extractNatural (Constructor { arguments: [], name: "Z" }) = Just 0
extractNatural _ = Nothing


instance Traversable Expression where
    traverse f (Var v) = f v <#> Var
    traverse f (Constructor { name, arguments })
        = arguments
        # traverse (traverse f)
        <#> \arguments -> Constructor { name, arguments }

    sequence a = sequenceDefault a