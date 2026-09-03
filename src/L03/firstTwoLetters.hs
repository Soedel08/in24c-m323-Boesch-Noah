firstTwoLetters :: String -> (Char, Char)
firstTwoLetters (x:y:_) = (x, y)
firstTwoLetters [x]     = (x, '?')
firstTwoLetters []      = ('?', '?')
