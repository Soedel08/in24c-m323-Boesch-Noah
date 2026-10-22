import Numeric.Natural (Natural)

-- Zahl an Position (n, k) im Pascalschen Dreieck (Binomialkoeffizient)
c :: Natural -> Natural -> Natural
c _ 0 = 1               -- linker Rand: immer 1
c n k
  | k == n    = 1        -- rechter Rand: immer 1
  | otherwise = c (n - 1) (k - 1) + c (n - 1) k
