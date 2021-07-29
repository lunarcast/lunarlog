module Lunarlog.Search.Naive where

import Loglude hiding (ask)

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.HashMap as HashMap
import Debug (traceM)
import Lunarlog.Core.NodeGraph as NodeGraph
import Lunarlog.Expression (Constructor, Rule, _constructorArgument, instantiateRule, runInstantiation)
import Lunarlog.Substitution (Substitution, mergeSubstitutions, substitute)
import Lunarlog.Unify (unifyMany)
import Run.Reader (ask)

type Rules = HashMap String (Array NodeGraph.Rule)
type Goals = Array (Constructor String)
type SOLVE r = READER Rules + SUPPLY Int r

getMatchingRules :: forall r. Constructor String -> Run (SOLVE r) (Array (Rule String /\ Substitution))
getMatchingRules constructor = do
    -- | Find all the rules with the correct name
    rules <- ask <#> HashMap.lookup constructor.name <#> fromMaybe []
    -- | Instantiate all the rules
    instantiated <- for rules (instantiateRule >>> runInstantiation)
    traceM instantiated
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
solve goals = do
    branches <- branch goals
    join <$> for branches \(subst /\ goals') -> ado
        solutions <- solve goals'
        -- TODO: perhaps try checking for cycles
        -- I saw other people do this, but am not sure why yet.
        -- Never came up in practice
        in mergeSubstitutions subst <$> solutions