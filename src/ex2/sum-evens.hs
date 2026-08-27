-- Summiert alle geraden Zahlen von 1 bis n
-- Pure Funktion: keine Schleife, keine Mutation - eine Kette aus Range, Filter und Sum
sumEvens :: Int -> Int
sumEvens n = sum (filter even [1..n])
