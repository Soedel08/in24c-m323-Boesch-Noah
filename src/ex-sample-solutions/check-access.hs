-- Haskell equivalents of check-access.js

checkAccess :: Int -> Bool -> Bool -> Bool
checkAccess age hasTicket isVIP = hasTicket && (age >= 18 || isVIP)

-- Alternative implementation using guards
-- First matching guard wins
checkAccess' :: Int -> Bool -> Bool -> Bool
checkAccess' age hasTicket isVIP
  | not hasTicket = False -- no ticket always denies access
  | isVIP         = True  -- VIP overrides the plain age check
  | age >= 18     = True
  | otherwise     = False

