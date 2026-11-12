{-# LANGUAGE GADTs #-}

import Sampledata
import Vector

-- ************* EXERCISE 6-1 *************

shiftPoint :: Vector -> Vector -> Vector
-- a1: horizontal shift
-- a2: vertical shift
-- b1: x-coordinate of the point to be shifted
-- b2: y-coordinate of the point to be shifted
shiftPoint (Vector a1 a2) (Vector b1 b2) = Vector (a1 + b1) (a2 + b2)

-- ************* EXERCISE 6-2 *************

shiftPoints :: Vector -> [Vector] -> [Vector]
shiftPoints vector = map (shiftPoint vector)

-- ************* EXERCISE 6-3 *************

distance :: Vector -> Vector -> Double
-- pythagoras
-- a1: x-coordinate of the first point
-- a2: y-coordinate of the first point
-- b1: x-coordinate of the second point
-- b2: y-coordinate of the second point
distance (Vector a1 a2) (Vector b1 b2) = sqrt ((b1 - a1) ^ 2 + (b2 - a2) ^ 2)

-- ************* EXERCISE 6-4 *************

fartherFromOrigin :: Vector -> Vector -> Vector
fartherFromOrigin a b
  | distance a (Vector 0 0) > distance b (Vector 0 0) = a
  | otherwise = b

-- ************* EXERCISE 6-5 *************

farthestFromOrigin :: [Vector] -> Vector
farthestFromOrigin = foldl fartherFromOrigin (Vector 0 0)

-- ************* EXERCISE 6-6 *************

data OrderStatus where
  Received :: OrderStatus
  Processing :: OrderStatus
  Shipped :: OrderStatus
  Delivered :: OrderStatus
  Cancelled :: OrderStatus
  -- Eq: automatically create instance of Eq type class, allows to check for equality
  -- Show: automatically create instance of Show type class, allows to convert to string
  deriving (Show, Eq)

countStatus :: OrderStatus -> [OrderStatus] -> Int
countStatus status = length . filter (== status)

-- ************* EXERCISE 6-7 *************

data Priority where
  Low :: Priority
  Medium :: Priority
  High :: Priority
  Critical :: Priority
  -- Ord: automatically create instance of Ord type class, allows to compare values
  -- Eq: automatically create instance of Eq type class, allows to check for equality
  -- Show: automatically create instance of Show type class, allows to convert to string
  deriving (Show, Eq, Ord)

highestPriority :: [Priority] -> Priority
highestPriority [] = Low
highestPriority (p : ps) = foldl max p ps

-- ************* EXERCISE 6-8 *************

data Weekday where
  Monday :: Weekday
  Tuesday :: Weekday
  Wednesday :: Weekday
  Thursday :: Weekday
  Friday :: Weekday
  Saturday :: Weekday
  Sunday :: Weekday
  -- Eq: automatically create instance of Eq type class, allows to check for equality
  -- Show: automatically create instance of Show type class, allows to convert to string
  deriving (Show, Eq)

-- ************* EXERCISE 6-9 *************

tomorrow :: Weekday -> Weekday
tomorrow Monday = Tuesday
tomorrow Tuesday = Wednesday
tomorrow Wednesday = Thursday
tomorrow Thursday = Friday
tomorrow Friday = Saturday
tomorrow Saturday = Sunday
tomorrow Sunday = Monday

-- ************* EXERCISE 6-10 *************

futureDay' :: Int -> Weekday -> Weekday
-- mod n 7: avoid unnecessary cycles if n >= 7
futureDay' n day = go (mod n 7) day
  where
    go :: Int -> Weekday -> Weekday
    go 0 day = day
    go n day = go (n - 1) (tomorrow day)

-- ************* EXERCISE 6-11 *************

futureDay :: Int -> Weekday -> Weekday
-- mod n 7: avoid unnecessary cycles if n >= 7
futureDay n day = iterate tomorrow day !! (mod n 7)

