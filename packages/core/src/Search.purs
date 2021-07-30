module Lunarlog.Search.Naive where

import Loglude hiding (ask)

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.HashMap as HashMap
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Expression (Constructor, Rule, _constructorArgument, instantiateRule, runInstantiation)
import Lunarlog.Substitution (Substitution, mergeSubstitutions, substitute)
import Lunarlog.Unify (unifyMany)
import Run.Except (fail)
import Run.Reader (Reader, ask, askAt, localAt, runReaderAt)

---------- Types
type Rules = HashMap String (Array NodeGraph.Rule)
type Goals = Array (Constructor String)

type DEPTH r = ( depth :: Reader (Int /\ Int) | r ) -- Reader (max depth /\ current depth)
type SOLVE r = READER Rules + SUPPLY Int + FAIL + DEPTH r

---------- Helpers
recurse :: forall r. Run (DEPTH + FAIL r) ~> Run (DEPTH + FAIL r)
recurse computation = do
    max /\ current <- askAt _depth
    if current >= max
        then fail
        else localAt _depth (second $ (+) 1) computation

runDepth :: forall r. Int -> Run (DEPTH r) ~> Run r
runDepth maxDepth = runReaderAt _depth (maxDepth /\ 0)

---------- Implementation
getMatchingRules :: forall r. Constructor String -> Run (SOLVE r) (Array (Rule String /\ Substitution))
getMatchingRules constructor = do
    -- | Find all the rules with the correct name
    rules <- ask <#> HashMap.lookup constructor.name <#> fromMaybe []
    -- | Instantiate all the rules
    instantiated <- for rules (instantiateRule >>> runInstantiation)
    -- | Unify the search term with the head of each rule to see which ones match
    pure $ flip Array.mapMaybe instantiated $ bindFlipped \rule -> 
        unifyMany constructor.arguments rule.head.arguments
        <#> \subst -> rule /\ subst 

branch :: forall r. Array (Constructor String) -> Run (SOLVE r) (Array (Substitution /\ Goals)) 
branch = Array.uncons >>> case _ of
    Nothing -> pure []
    Just { head, tail } -> do
        rules <- getMatchingRules head
        pure $ rules <#> \(rule /\ subst) -> subst /\ map (over _constructorArgument $ substitute subst) (rule.body <> tail)

solve :: forall r. Goals -> Run (SOLVE r) (Array Substitution)
solve [] = pure [HashMap.empty]
solve goals = recurse do
    branches <- branch goals
    join <$> for branches \(subst /\ goals') -> ado
        solutions <- solve goals'
        -- TODO: perhaps try checking for cycles
        -- I saw other people do this, but am not sure why yet.
        -- Never came up in practice
        in mergeSubstitutions subst <$> solutions

---------- Proxies
_depth :: Proxy "depth"
_depth = Proxy