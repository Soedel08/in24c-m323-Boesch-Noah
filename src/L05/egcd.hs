-- Grösster gemeinsamer Teiler via Euklidischem Algorithmus
egcd :: Integral a => a -> a -> a
egcd p 0 = p
egcd p q = egcd q (p `mod` q)
