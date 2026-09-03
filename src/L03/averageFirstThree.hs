averageFirstThree :: [Double] -> Double
averageFirstThree (x:y:z:_) = (x + y + z) / 3
averageFirstThree _         = error "List must contain at least three elements"
