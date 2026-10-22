import Numeric.Natural (Natural)

fibseq :: Natural -> [Natural]
fibseq n = reverse (build n)
  where
    build 0 = [0]
    build 1 = [1, 0]
    build k = let bs@(b1 : b2 : _) = build (k - 1) in (b1 + b2) : bs

-- Fibonacci-Zahl an Position n
fib :: Natural -> Natural
fib n = last (fibseq n)
