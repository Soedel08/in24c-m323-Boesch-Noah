sumOfEvenSquares :: Int -> Int
sumOfEvenSquares n = sum [x^2 | x <- [1..n], even x]
