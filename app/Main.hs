module Main where

import Data.List

type Matrix = [[Int]] -- matrix type

type Pos = (Int, Int) -- index type

(|!!|) :: (Integral i1, Integral i2) => [[Int]] -> (i1, i2) -> Int -- get element by index
m |!!| (x, y) = m `genericIndex` x `genericIndex` y

(|!|) :: (Integral i1) => Matrix -> i1 -> [Int] -- get row by index
m |!| ind = m `genericIndex` ind

(|+|) :: Matrix -> Matrix -> Matrix -- matrix sum
_ |+| [] = []
[] |+| _ = []
(r1 : rs1) |+| (r2 : rs2) = sumRow r1 r2 : (rs1 |+| rs2)
  where
    sumRow = zipWith (+)

(|-|) :: Matrix -> Matrix -> Matrix -- matrix dif
_ |-| [] = []
[] |-| _ = []
(r1 : rs1) |-| (r2 : rs2) = minusRow r1 r2 : (rs1 |+| rs2)
  where
    minusRow = zipWith (-)

(|*|) :: Matrix -> Matrix -> Matrix -- matrix mult
a |*| b = map (map calcCell) indexes
  where
    b' = transpose b
    indexes = [[(x, y) | y <- [0 .. (length b' - 1)]] | x <- [0 .. (length a - 1)]]
    rowMult a b = sum (zipWith (*) a b)
    calcCell (i, j) = rowMult (a !! i) (b' !! j)

(*|) :: Int -> Matrix -> Matrix -- matrix mult on scalar
a *| b = map (map (* a)) b

tr :: Matrix -> Int -- matrix sled
tr m = go 0 m
  where
    go ind (m : ms) = m !! ind + go (ind + 1) ms
    go _ [] = 0


gauss :: Matrix -> Matrix
gauss m = foldl solveColumn m [0 .. (length (head m) - 2)]
  where
    solveColumn mat i = case mRow of
      Nothing -> mat
      Just row -> row : map (rowToZero row) (filter (/= row) mat)
      where
        mRow = find (\a -> (a !! i) /= 0) (drop i mat)
        rowToZero a b = case elb of
          0 -> b
          _ -> zipWith (-) (map (* mulb) b) (map (* mula) a)
          where
            ela = a !! i
            elb = b !! i
            mula = lcm ela elb `div` ela
            mulb = lcm ela elb `div` elb

main :: IO ()
main = print (gauss [[9, 9, -27, 18, 54], [7, -4, -10, 14, 20], [1, -5, 3, 2, -6], [8, -7, -9, 16, 18]])
