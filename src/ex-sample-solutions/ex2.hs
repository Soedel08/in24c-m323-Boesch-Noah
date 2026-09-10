-- sample data, can be used for the exercises (= function that returns a fixed list of users)
-- users: list of users (tuple of integers, strings, strings, strings)
users :: [(Int, String, String, String)]
users = [(101, "Wirth", "Niklaus", "nw@inf.ethz.ch"), (102, "Booch", "Grady", "gbo@oonet.com"), (103, "Ritchie", "Dennis", "dr@bell.org")]

-- get the price of a movie
-- argument: (String, Int, Double) - movie title, number of movie theaters, price (tuple of strings, integers, doubles)
getMoviePrice :: (String, Int, Double) -> Double
getMoviePrice (_, _, price) = price

-- check if a point is the origin (0, 0)
-- argument: (Int, Int) - point (tuple of integers)
isOrigin :: (Int, Int) -> Bool
isOrigin (0, 0) = True
isOrigin (_, _) = False

-- average of the first three elements of a list
-- argument: [Double] - list of doubles
averageFirstThree :: [Double] -> Double
-- pattern: at least three elements in the list
averageFirstThree (x : y : z : _) = (x + y + z) / 3
-- pattern: less than three elements in the list
averageFirstThree _ = error "List has less than three elements"

-- get the first two letters of a string
-- argument: String - string
firstTwoLetters :: String -> (Char, Char)
-- pattern: at least two elements in the string
firstTwoLetters (x : y : _) = (x, y)
-- pattern: one element in the string
firstTwoLetters (x : []) = (x, '?')
-- pattern: no elements in the string (fallback)
firstTwoLetters _ = ('?', '?')

-- check the signal status of a list of booleans
-- argument: [Bool] - list of booleans
checkSignalStatus :: [Bool] -> String
-- pattern: True : False : _ - warning
checkSignalStatus (True : False : _) = "Warnung"
-- pattern: True : True : _ - stable
checkSignalStatus (True : True : _) = "Stabil"
-- pattern: _ - unknown (fallback)
checkSignalStatus _ = "Unbekannt"

-- get the first user from a list of users (tuple of integers, strings, strings, strings)
-- argument: [(Int, String, String, String)] - list of users (tuple of integers, strings, strings, strings)
firstUser :: [(Int, String, String, String)] -> (Int, String)
-- pattern: (id, _, _, username) : _ - get the first user
firstUser ((id, _, _, username) : _) = (id, username)
-- pattern: _ - error (fallback)
firstUser _ = error "List is empty"

-- sum of squares of even numbers up to n
sumOfEvenSquares :: Int -> Int
-- list comprehension: [x ^ 2 | x <- [1 .. n], even x]
-- sum: sum the numbers in the list
-- even x: check if x is even
sumOfEvenSquares n = sum [x ^ 2 | x <- [1 .. n], even x]

-- list of points on the plane, no diagonal points
noDiagonalPoints :: Int -> [(Int, Int)]
-- list comprehension: [(x, y) | x <- [0 .. n], y <- [0 .. n], x /= y]
-- x /= y: check if x is not equal to y (no diagonal points, predicate)
noDiagonalPoints n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x /= y]
