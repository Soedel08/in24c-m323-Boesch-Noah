noDiagonalPoints :: Int -> [(Int, Int)]
noDiagonalPoints n = [(x, y) | x <- [0..n], y <- [0..n], x/=y]