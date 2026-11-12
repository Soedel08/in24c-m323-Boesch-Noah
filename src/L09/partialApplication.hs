import Data.Char (toUpper)

keepEvens :: [Int] -> [Int]
keepEvens = filter even

sumSquares :: [Int] -> Int
sumSquares = foldl (+) 0 . map (^2)

filterMinLength :: Int -> [String] -> [String]
filterMinLength n = filter ((>= n) . length)

cleanAndUppercase :: [String] -> [String]
cleanAndUppercase = map (map toUpper . filter (/= ' '))

productOfPositives :: [Int] -> Int
productOfPositives = foldl (*) 1 . filter (>0)

countChar :: Char -> String -> Int
countChar c = foldl (+) 0 . map (const 1) . filter (== c)

processPoints :: [(Int, Int)] -> [Int]
processPoints = map (\(x, y) -> x - y) . filter (\(x, y) -> x + y > 10)

processPoints2 :: [(Int, Int)] -> [Int]
processPoints2 = map (uncurry (-)) . filter ((>10) . uncurry (+))

average :: [Double] -> Double
average xs = foldl (+) 0 xs / fromIntegral (length xs)

{-
weightedAverage :: [(Double, Double)] -> Double
weightedAverage = foldl (+) 0 (\(x,y) -> x * y)-}
