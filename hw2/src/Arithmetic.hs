module Arithmetic
       ( Expr(..)
       , ArithmeticError(..)
       , eval
       , bin
       ) where

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError = DivisionByZero
                     | NegativePow
                     deriving (Show, Eq)

apply :: Expr -> Expr -> (Int -> Int -> Either ArithmeticError Int) -> Either ArithmeticError Int
apply left right f = eval left >>= (\lres -> eval right >>= f lres)

eval :: Expr -> Either ArithmeticError Int
eval (Const value)    = Right value
eval (Add left right) = apply left right (\l r -> Right (l + r))
eval (Sub left right) = apply left right (\l r -> Right (l - r))
eval (Mul left right) = apply left right (\l r -> Right (l * r))
eval (Div left right) = apply left right division
  where
    division :: Int -> Int -> Either ArithmeticError Int
    division _ 0 = Left DivisionByZero
    division l r = Right (l `div` r)
eval (Pow left right) = apply left right power
  where
    power :: Int -> Int -> Either ArithmeticError Int
    power l r
        | r < 0 = Left NegativePow
        | otherwise = Right (l ^ r)

bin :: Int -> [[Int]]
bin n
    | n < 0 = []
    | n == 1 = [[0], [1]]
    | otherwise = bin (n - 1) >>= (\list -> [0 : list, 1 : list])
