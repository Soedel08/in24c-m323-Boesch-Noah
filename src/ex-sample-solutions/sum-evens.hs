-- Haskell equivalent of sum-evens.js
-- The JS mutates 'total' in a loop, adding 'i' whenever it is even.
-- Haskell has no mutable loop variable or accumulator: instead we build
-- the list of numbers from 1 to n, keep only the even ones (a list
-- comprehension guard, analogous to the 'if' in the JS loop body),
-- and fold them into a single value with 'sum'.

sumEvens :: Int -> Int
sumEvens n = sum [i | i <- [1 .. n], even i]

-- Alternative implementation using explicit recursion instead of a
-- list comprehension. Each recursive call plays the role of one loop
-- iteration; instead of mutating 'total', the running sum is simply
-- returned as part of the result of each call.
sumEvens' :: Int -> Int
sumEvens' n
  | n <= 0    = 0
  | even n    = n + sumEvens' (n - 1)
  | otherwise = sumEvens' (n - 1)
