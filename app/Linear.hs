module Linear where

import Data.List
import Data.Maybe

data Variable = Var Int | Free deriving (Eq)

isFree :: Variable -> Bool
isFree (Var _) = False
isFree Free = True

instance Show Variable where
  show (Var id) = "x_" ++ show id
  show Free = ""

newtype LineEq = Eq [(Variable, Int)]

instance Show LineEq where
  show (Eq list) = unwords (map showVar list)
    where
      showVar (v, coef) = if coef > 0 then "+" ++ show coef ++ show v else show coef ++ show v

def = Eq [(Free, 0)]

variables :: LineEq -> [Variable]
variables (Eq x) = map fst x

getCoef :: LineEq -> Variable -> Int
getCoef (Eq x) v = fromMaybe 0 (lookup v x)

fix (Eq []) = Eq [(Free, 0)]
fix (Eq a) = Eq a

(^+^) :: LineEq -> LineEq -> LineEq -- котик
a ^+^ b = fix (Eq (filter (\x -> snd x /= 0) (zip vars (map sumCoef vars))))
  where
    vars = variables a `union` variables b
    sumCoef x = getCoef a x + getCoef b x

(^*) :: LineEq -> Int -> LineEq -- правый полукрот
(Eq a) ^* 0 = Eq [(Free, 0)]
(Eq a) ^* b = Eq (map (\(v, c) -> (v, c * b)) a)

(*^) :: Int -> LineEq -> LineEq -- левый полукрот
b *^ a = a ^* b

(^-^) :: LineEq -> LineEq -> LineEq -- смайлик
a ^-^ Eq b = Eq (map (\(v, coef) -> (v, -coef)) b)

(^-) :: LineEq -> Int -> LineEq -- левый полусмайл
a ^- x = a ^-^ Eq [(Free, x)]

(-^) :: Int -> LineEq -> LineEq -- левый полусмайл
x -^ a = a ^- x
