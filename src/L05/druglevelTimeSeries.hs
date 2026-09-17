import Numeric.Natural (Natural)

-- Zeitreihe der Medikamentenspiegelwerte, Tag t am Anfang der Liste, Tag 0 am Ende
druglevelTimeSeries :: (Fractional a) => a -> a -> Natural -> [(Natural, a)]
druglevelTimeSeries _ _ 0 = [(0, 0)]
druglevelTimeSeries m r t = (t, druglevel m r t) : druglevelTimeSeries m r (t - 1)

-- Restmenge am Ende von Tag t
-- Modell: zu Tagesbeginn Einnahme m, im Laufe des Tages Abbau von r Prozent
druglevel :: (Fractional a) => a -> a -> Natural -> a
druglevel _ _ 0 = 0
druglevel m r t = (druglevel m r (t - 1) + m) * (1 - r / 100)
