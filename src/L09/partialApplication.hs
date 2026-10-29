keepEvens :: [Int] -> [Int]
keepEvens = filter even

sumSquares :: [Int] -> Int
sumSquares = foldl (+) 0 . map (^2)