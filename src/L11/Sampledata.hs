module Sampledata where

import Vector

-- Beispielpunkte zum Testen der Übungen 6-1 bis 6-5
points :: [Vector]
points = [Vector 1 2, Vector (-3) 4, Vector 0.5 (-0.5), Vector 6 (-8)]

-- Beispiel-Translationsvektor: 2 nach rechts, 1 nach unten
translation :: Vector
translation = Vector 2 (-1)
