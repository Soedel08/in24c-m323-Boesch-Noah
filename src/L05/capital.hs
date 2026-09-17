import Numeric.Natural (Natural)

-- Kapital am Jahresende nach t Jahren
-- Modell: zu Jahresbeginn Einlage d, am Jahresende Verzinsung des gesamten Kapitals mit i Prozent
capital :: Fractional a => a -> a -> Natural -> a
capital _ _ 0 = 0
capital d i t = (capital d i (t - 1) + d) * (1 + i / 100)
