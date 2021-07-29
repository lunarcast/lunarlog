module Lunarlog.Substitution where

import Loglude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap as HashMap
import Lunarlog.Expression (Expression(..))

type Substitution = HashMap String (Expression String)

substitute :: Substitution -> Expression String -> Expression String
substitute subst v@(Var name) = fromMaybe v $ HashMap.lookup name subst 
substitute subst (Constructor pattern) = Constructor pattern { arguments = map (substitute subst) pattern.arguments }

-- | More or less `append` for substitutions
mergeSubstitutions :: Substitution -> Substitution -> Substitution
mergeSubstitutions s1 s2 = foldlWithIndex (\name subst term -> insert name term subst) s1 s2

-- | Insert a new var-term pair into a substitution
insert :: String -> Expression String -> Substitution -> Substitution
insert name term s = HashMap.insert name term $ map updateMap s
    where
    updateMap = substitute $ HashMap.singleton name term
