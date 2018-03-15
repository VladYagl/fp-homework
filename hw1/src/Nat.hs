module Nat
       ( Nat(..)
       , isEven
       , isOdd
       ) where

data Nat = Z | S Nat

instance Num Nat where
    Z + second = second
    (S first) + second = S (first + second)

    Z * _              = Z
    (S first) * second = second + first * second

    second - Z = second
    (S first) - (S second) = first - second
    Z - _ = Z

    abs a = a

    signum Z = 0
    signum _ = 1

    fromInteger 0 = Z
    fromInteger i = S (fromInteger (i - 1))

instance Eq Nat where
    Z == Z = True
    Z == _ = False
    _ == Z = False
    S a == S b = a == b

instance Ord Nat where
    a <= b = a - b == Z

toInt :: Nat -> Int
toInt Z     = 0
toInt (S a) = 1 + toInt a

instance Real Nat where
    toRational a = toRational (toInt a)

instance Enum Nat where
    toEnum a = fromInteger (toInteger a)

    fromEnum = toInt

instance Integral Nat where
    toInteger = toInteger . toInt

    quot _ Z = error "division by Z"
    quot a b
        | a < b = Z
        | otherwise = S Z + quot (a - b) b

    rem _ Z = error "division by Z"
    rem a b
        | a < b = a
        | otherwise = rem (a - b) b

    quotRem a b = (quot a b, rem a b)


isEven, isOdd :: Nat -> Bool
isEven Z     = True
isEven (S a) = not (isEven a)

isOdd = not . isEven


