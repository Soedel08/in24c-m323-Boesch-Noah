import Numeric.Natural (Natural)

fibseq :: Natural -> [Natural]
fibseq n = reverse (build n)
  where
    build 0 = [0]
    build 1 = [1, 0]
    build k = let bs@(b1 : b2 : _) = build (k - 1) in (b1 + b2) : bs

-- Position und zugehörige Fibonacci-Zahl als Liste von Paaren
fiblist :: Natural -> [(Natural, Natural)]
fiblist n = zip [0 .. n] (fibseq n)
