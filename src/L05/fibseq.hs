import Numeric.Natural (Natural)

-- Erste n+1 Fibonacci-Zahlen (Position 0 bis n) als Liste, aufsteigend
fibseq :: Natural -> [Natural]
fibseq n = reverse (build n)
  where
    -- baut die Liste absteigend via prepend (:), nie mit (++) anhängen
    build 0 = [0]
    build 1 = [1, 0]
    build k = let bs@(b1 : b2 : _) = build (k - 1) in (b1 + b2) : bs
