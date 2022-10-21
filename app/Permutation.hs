module Permutation where

type Permutation = [Int]

inverses :: Permutation -> Int
inverses [] = 0
inverses (x:xs) = length (filter (<x) xs) + inverses xs

isEven :: Permutation -> Bool
isEven x = even (inverses x)