module Weekday
       ( Weekday(..)
       , afterDays
       , nextDay
       , isWeekend
       , daysToParty
       ) where

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Show)

afterDays :: Weekday -> Int -> Weekday
afterDays day count = toEnum ((fromEnum day + count) `mod` 7)

nextDay :: Weekday -> Weekday
nextDay = flip afterDays 1

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Weekday -> Int
daysToParty day = (fromEnum Friday - fromEnum day) `mod` 7
