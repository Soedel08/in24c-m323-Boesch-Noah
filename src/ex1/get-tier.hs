-- Ermittelt die Einstufung (Tier) aus einem Score
-- Pure Funktion: keine Mutation, Guards statt if-else-Kette
getTier :: Int -> Char
getTier score
    | score >= 90 = 'A'
    | score >= 75 = 'B'
    | otherwise   = 'C'
