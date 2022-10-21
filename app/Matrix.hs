module Matrix where

import Data.List
import Linear

type Matrix a = [[a]] -- matrix type

type Pos = (Int, Int) -- index type

(|!!|) :: (Integral i1, Integral i2) => Matrix a -> (i1, i2) -> a -- get element by index
m |!!| (x, y) = m `genericIndex` x `genericIndex` y

(|!|) :: (Integral i1) => Matrix a -> i1 -> [a] -- get row by index
m |!| ind = m `genericIndex` ind

(|+|) :: Matrix Int -> Matrix Int -> Matrix Int -- matrix sum
_ |+| [] = []
[] |+| _ = []
(r1 : rs1) |+| (r2 : rs2) = sumRow r1 r2 : (rs1 |+| rs2)
  where
    sumRow = zipWith (+)

(|-|) :: Matrix Int -> Matrix Int -> Matrix Int -- matrix dif
_ |-| [] = []
[] |-| _ = []
(r1 : rs1) |-| (r2 : rs2) = minusRow r1 r2 : (rs1 |+| rs2)
  where
    minusRow = zipWith (-)

(|*|) :: Matrix Int -> Matrix Int -> Matrix Int -- matrix mult
a |*| b = map (map calcCell) indexes
  where
    b' = transpose b
    indexes = [[(x, y) | y <- [0 .. (length b' - 1)]] | x <- [0 .. (length a - 1)]]
    rowMult a b = sum (zipWith (*) a b)
    calcCell (i, j) = rowMult (a !! i) (b' !! j)

(*|) :: Int -> Matrix Int -> Matrix Int -- matrix mult on scalar
a *| b = map (map (* a)) b

tr :: Matrix Int -> Int -- matrix sled
tr m = go 0 m
  where
    go ind (m : ms) = m !! ind + go (ind + 1) ms
    go _ [] = 0

gauss' :: Int -> Matrix Int -> Matrix Int
gauss' cols m = foldl solveColumn m [0 .. cols] -- gauss
  where
    solveColumn mat i = case mRow of -- in column i only 1 not zero element
      Nothing -> mat
      Just row -> row : map (rowToZero row) (filter (/= row) mat) -- all rows that not equals to r, substract r
      where
        mRow = find (\a -> (a !! i) /= 0) (drop i mat) -- find row with not zero element
        rowToZero a b = case elb of -- row a to zero with row b
          0 -> b --
          _ -> zipWith (-) (map (* mulb) b) (map (* mula) a) -- to lcm
          where
            ela = a !! i
            elb = b !! i
            mula = lcm ela elb `div` ela
            mulb = lcm ela elb `div` elb


gauss :: Matrix Int -> Matrix Int
gauss m = gauss' (length (head m) - 2) m

(|*^|) :: Matrix Int -> Matrix LineEq -> Matrix LineEq -- КОТИК В КОРОБКЕ
a |*^| b = map (map calcCell) indexes
  where
    b' = transpose b
    indexes = [[(x, y) | y <- [0 .. (length b' - 1)]] | x <- [0 .. (length a - 1)]]
    rowMult a b = foldl (^+^) def (zipWith (*^) a b)
    calcCell (i, j) = rowMult (a !! i) (b' !! j)

(|^*|) :: Matrix LineEq -> Matrix Int -> Matrix LineEq -- КОТИК В КОРОБКЕ
a |^*| b = map (map calcCell) indexes
  where
    b' = transpose b
    indexes = [[(x, y) | y <- [0 .. (length b' - 1)]] | x <- [0 .. (length a - 1)]]
    rowMult a b = foldl (^+^) def (zipWith (^*) a b)
    calcCell (i, j) = rowMult (a !! i) (b' !! j)



-- solveMatrixEquation :: Matrix LineEq -> Matrix LineEq -> Matrix Int
-- solveMatrixEquation a b = zipWith ^-^ (concat a) (concat b)