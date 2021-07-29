module Lunarlog.Unify (unify, unifyMany) where

import Loglude

import Data.Array (any, foldM, length)
import Data.Array as Array
import Data.HashMap as HashMap
import Lunarlog.Expression (Expression(..))
import Lunarlog.Substitution (Substitution, mergeSubstitutions, substitute)

-- | Check if an expression references a variable
occursCheck :: String -> Expression String -> Boolean
occursCheck target (Constructor { arguments }) = any (occursCheck target) arguments
occursCheck target (Var v) = v == target

-- | Bind a variable to a term, returning a substitution
bindVar :: String -> Expression String -> Maybe Substitution
bindVar name term 
    | occursCheck name term = Nothing
    | otherwise = Just $ HashMap.singleton name term

-- | Unification is a relation which has the goal of making two expressions look identical using a substitution
unify :: Expression String -> Expression String -> Maybe Substitution
unify (Var a) (Var b) | a == b = pure HashMap.empty 
unify (Var a) b = bindVar a b 
unify a (Var b) = bindVar b a 
unify (Constructor a) (Constructor b)
    | a.name /= b.name = Nothing 
    | otherwise = unifyMany a.arguments b.arguments

-- | Perform unification on more than one pair of expressions
unifyMany :: Array (Expression String) -> Array (Expression String) -> Maybe Substitution
unifyMany left right 
    | length left == length right = foldM go HashMap.empty (Array.zip left right)
    where
    go subst (a /\ b) = ado
        next <- unify (substitute subst a) (substitute subst b)
        in subst `mergeSubstitutions` next
    | otherwise = Nothing