module Lunarlog.Tea where


data Tree a 
    = Branch a (Array (Tree a))
    | Leaf a

