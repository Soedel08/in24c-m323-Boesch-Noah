{-# OPTIONS_GHC -Wno-x-partial #-}

import Numeric.Natural (Natural)

-- function capital
-- d: amount of regular deposits
-- i: interest rate (percentage)
-- t: duration
-- the function is defined by the base case and the recursive case
-- the base case is when t is 0, in which case the function returns 0
-- the recursive case is when t is greater than 0, in which case the function returns the previous capital plus the new deposit plus the yield
-- the yield is calculated as the previous capital plus the new deposit multiplied by the interest rate
-- the interest rate is divided by 100 to convert it to a fraction
-- the previous capital is calculated by calling the function recursively with t - 1
-- the new deposit is the regular deposit
-- returns the total capital after t years
capital :: (Fractional a) => a -> a -> Natural -> a
-- base case: when t is 0, return 0
-- break off the recursion when t is 0
capital _ _ 0 = 0
-- recursive case: when t is greater than 0, return the previous capital plus the new deposit plus the yield
capital d i t = previousCapital + d + yield
  where
    previousCapital = capital d i (t - 1)
    yield = (previousCapital + d) * (i / 100)

-- m: amount of drug taken per day
-- r: reduction rate
-- t: number of days
druglevelTimeSeries :: (Fractional a, Integral b) => a -> a -> b -> [(b, a)]
-- base case: at day 0, the drug level is 0
-- break off the recursion when t is 0
druglevelTimeSeries _ _ 0 = [(0, 0)]
druglevelTimeSeries m r t = (t, currentDayLevel) : perviousDayList
  where
    perviousDayList = druglevelTimeSeries m r (t - 1)
    perviousDayLevel = snd (head perviousDayList)
    currentDayLevel = (perviousDayLevel + m) * (1 - r / 100)

-- p: first number
-- q: second number
egcd :: (Integral a) => a -> a -> a
egcd p q
  | (p == 0) || (q == 0) = error "ggT nicht definiert"
  -- base case: when the remainder is 0, return the smaller of p and q
  -- break off the recursion when the remainder is 0
  | remainder == 0 = n
  -- recursive case: when the remainder is not 0, call the function recursively with n and remainder
  | otherwise = egcd n remainder
  where
    m = max (abs p) (abs q)
    n = min (abs p) (abs q)
    remainder = mod m n
