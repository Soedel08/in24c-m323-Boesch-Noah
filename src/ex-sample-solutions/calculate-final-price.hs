-- calculate the final price of a product
-- guards on the Bool (isMember) perform case distinction
calculateFinalPrice :: Double -> Bool -> Double
calculateFinalPrice basePrice isMember
  | isMember = ((basePrice * memberDiscount) + processingFee) * salesTax
  | otherwise = (basePrice + processingFee) * salesTax
  where
    processingFee = 5.0 -- flat $5 processing fee
    salesTax = 1.08 -- 8% sales tax
    memberDiscount = 0.90 -- 10% member discount