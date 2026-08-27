-- Haskell equivalent of get-tier.js
-- The JS mutates 'tier' through an if/else-if chain (highest score wins,
-- checked first). Guards express that same top-to-bottom priority
-- directly, so the order here matches the JS order (unlike checkAccess',
-- where the JS reassignments overrode each other and had to be reversed).

getTier :: Int -> Char
getTier score
  | score >= 90 = 'A'
  | score >= 75 = 'B'
  | otherwise   = 'C' -- default tier
