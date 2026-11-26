{-# LANGUAGE GADTs #-}

-- =====================================================================
-- Eigene Datentypen mit `data` (algebraische Datentypen)
-- =====================================================================
-- Mit `data` definiert man einen NEUEN Typ. Es gibt zwei Grundformen:
--
-- 1. Produkttyp: ein Wert besteht aus mehreren Teilen GLEICHZEITIG (x UND y).
--    Beispiel: Vector – ein Punkt hat immer eine x- UND eine y-Koordinate.
--    Ähnlich wie ein Tupel (Double, Double), aber mit eigenem Namen:
--    der Compiler verwechselt einen Vector nicht mit irgendeinem anderen Paar.
--
-- 2. Summentyp: ein Wert ist GENAU EINE von mehreren Varianten
--    (Received ODER Processing ODER ...). Vergleichbar mit einem enum.
--
-- Schreibweise (GADT-Syntax wie in den Folien, braucht die GADTs-Pragma oben):
--   data Typname where
--     Konstruktor :: Feldtyp1 -> Feldtyp2 -> Typname
--
-- Ein Konstruktor ist einfach eine Funktion, die einen Wert des Typs baut:
--   Vector :: Double -> Double -> Vector
--   Vector 3 4        -- ein Vector mit x = 3, y = 4
--
-- Klassische Schreibweise, bedeutet exakt dasselbe:
--   data Vector = Vector Double Double deriving (Show, Eq)
--
-- deriving (...) lässt den Compiler Instanzen von Type Classes automatisch erzeugen:
--   Show -> Wert kann als Text angezeigt werden (nötig für die Ausgabe in GHCi)
--   Eq   -> Werte können mit == und /= verglichen werden
--   Ord  -> Werte haben eine Reihenfolge (<, >, max, maximum, ...)

-- ---------------------------------------------------------------------
-- Produkttyp Vector
-- ---------------------------------------------------------------------
-- (In der Musterlösung steckt derselbe Typ im Modul src/Vector.hs,
-- hier direkt in der Datei, damit alles an einem Ort ist.)

data Vector where
  Vector :: Double -> Double -> Vector
  deriving (Show, Eq)

-- Koordinatenursprung, wird in mehreren Übungen gebraucht
origin :: Vector
origin = Vector 0 0

-- ************* AUFGABE 6-1 *************
-- Punkt um einen Translationsvektor verschieben.
--
-- Pattern Matching auf einem Konstruktor funktioniert genau wie bei Tupeln:
-- (Vector tx ty) zerlegt den Vector in seine beiden Felder.
-- Verschieben = komponentenweise addieren: x + tx, y + ty.
--
-- ghci> shiftPoint (Vector 2 (-1)) (Vector 1 2)
-- Vector 3.0 1.0

shiftPoint :: Vector -> Vector -> Vector
shiftPoint (Vector tx ty) (Vector x y) = Vector (x + tx) (y + ty)

-- ************* AUFGABE 6-2 *************
-- Eine ganze Liste von Punkten verschieben.
--
-- shiftPoint t ist partial application: shiftPoint mit nur dem 1. Argument
-- ergibt eine Funktion Vector -> Vector, "verschiebe einen Punkt um t".
-- map wendet diese Funktion auf jeden Punkt der Liste an.
-- Point-free: die Punktliste wird nicht benannt (wie bei doubleAll).
--
-- ghci> shiftPoints (Vector 2 (-1)) [Vector 1 2, Vector 0 0]
-- [Vector 3.0 1.0,Vector 2.0 (-1.0)]

shiftPoints :: Vector -> [Vector] -> [Vector]
shiftPoints t = map (shiftPoint t)

-- ************* AUFGABE 6-3 *************
-- Distanz zweier Punkte mit Pythagoras: c = sqrt (a² + b²)
-- a = Differenz der x-Koordinaten, b = Differenz der y-Koordinaten.
--
-- ghci> distance (Vector 0 0) (Vector 3 4)
-- 5.0

distance :: Vector -> Vector -> Double
distance (Vector ax ay) (Vector bx by) = sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)

-- ************* AUFGABE 6-4 *************
-- Den Punkt zurückgeben, der weiter vom Ursprung entfernt ist.
-- Guards: Bedingung prüfen, erste zutreffende gewinnt.
-- Bei gleicher Distanz wird b zurückgegeben (otherwise).
--
-- ghci> fartherFromOrigin (Vector 1 1) (Vector (-3) 4)
-- Vector (-3.0) 4.0

fartherFromOrigin :: Vector -> Vector -> Vector
fartherFromOrigin a b
  | distance origin a > distance origin b = a
  | otherwise = b

-- ************* AUFGABE 6-5 *************
-- Den Punkt einer Liste finden, der am weitesten vom Ursprung liegt.
--
-- foldl mit fartherFromOrigin funktioniert wie ein "Champion"-Prinzip:
-- Der Akkumulator ist der bisher weiteste Punkt. Jeder neue Punkt tritt
-- gegen ihn an, der weitere bleibt als neuer Akkumulator übrig.
--
--   foldl fartherFromOrigin origin [p1, p2, p3]
--   = fartherFromOrigin (fartherFromOrigin (fartherFromOrigin origin p1) p2) p3
--
-- Startwert origin: hat Distanz 0, wird also vom ersten echten Punkt sofort
-- abgelöst. Leere Liste -> origin.
--
-- ghci> farthestFromOrigin [Vector 1 2, Vector (-3) 4, Vector 6 (-8)]
-- Vector 6.0 (-8.0)

farthestFromOrigin :: [Vector] -> Vector
farthestFromOrigin = foldl fartherFromOrigin origin

-- =====================================================================
-- Summentypen
-- =====================================================================

-- ************* AUFGABE 6-6 *************
-- Summentyp: ein OrderStatus ist GENAU EINER dieser fünf Werte.
-- Die Konstruktoren haben keine Felder (keine Pfeile in der Signatur),
-- sie SIND direkt die Werte.
--
-- deriving Eq ist hier nötig, weil countStatus mit (== status) vergleicht.
-- Ohne Eq weiss der Compiler nicht, wann zwei Status "gleich" sind.

data OrderStatus where
  Received :: OrderStatus
  Processing :: OrderStatus
  Shipped :: OrderStatus
  Delivered :: OrderStatus
  Cancelled :: OrderStatus
  deriving (Show, Eq)

-- Anzahl eines bestimmten Status zählen:
-- erst alle passenden Status behalten (filter), dann zählen (length).
-- Gleiches Prinzip wie countChar aus Lektion 9.
--
-- ghci> countStatus Shipped [Received, Shipped, Delivered, Shipped]
-- 2

countStatus :: OrderStatus -> [OrderStatus] -> Int
countStatus status = length . filter (== status)

-- ************* AUFGABE 6-7 *************
-- Warum wird Ord benötigt?
--   Um die "höchste" Priorität zu finden, muss man Prioritäten miteinander
--   VERGLEICHEN können (grösser / kleiner). Genau das liefert die Type Class
--   Ord (max, maximum, <, >). Ohne Ord meldet der Compiler:
--   "No instance for (Ord Priority)".
--
-- Bei deriving Ord gilt die Reihenfolge der Deklaration:
--   der zuerst genannte Konstruktor ist der kleinste.
--   Low < Medium < High < Critical
--
-- Show: damit GHCi z.B. "High" ausgeben kann.
-- Eq:   damit z.B. High == High funktioniert (Ord setzt Eq voraus).

data Priority where
  Low :: Priority
  Medium :: Priority
  High :: Priority
  Critical :: Priority
  deriving (Show, Eq, Ord)

-- Höchste Priorität einer Liste.
-- (p : ps): das erste Element dient als Startwert für foldl,
-- max vergleicht dann Schritt für Schritt (gleiches Champion-Prinzip wie 6-5).
-- Leere Liste: es gibt keine höchste Priorität, hier wird Low angenommen.
-- Alternative mit der eingebauten Funktion: maximum ps
-- (crasht aber bei leerer Liste, siehe Spick: maximum ist partiell).
--
-- ghci> highestPriority [Low, High, Medium]
-- High
-- ghci> Low < Critical
-- True

highestPriority :: [Priority] -> Priority
highestPriority [] = Low
highestPriority (p : ps) = foldl max p ps

-- ************* AUFGABE 6-8 *************
-- Summentyp mit den sieben Wochentagen.

data Weekday where
  Monday :: Weekday
  Tuesday :: Weekday
  Wednesday :: Weekday
  Thursday :: Weekday
  Friday :: Weekday
  Saturday :: Weekday
  Sunday :: Weekday
  deriving (Show, Eq)

-- ************* AUFGABE 6-9 *************
-- Nächster Wochentag: pro Konstruktor eine Gleichung (Pattern Matching).
-- Wichtig ist der Umbruch am Ende der Woche: Sunday -> Monday.
--
-- ghci> tomorrow Sunday
-- Monday

tomorrow :: Weekday -> Weekday
tomorrow Monday = Tuesday
tomorrow Tuesday = Wednesday
tomorrow Wednesday = Thursday
tomorrow Thursday = Friday
tomorrow Friday = Saturday
tomorrow Saturday = Sunday
tomorrow Sunday = Monday

-- ************* AUFGABE 6-10 *************
-- Wochentag nach n Tagen, mit Tail Recursion.
--
-- Tail Recursion = der rekursive Aufruf ist das ALLERLETZTE, was passiert.
-- Nach der Rückkehr des Aufrufs wird nichts mehr gerechnet.
--
--   tail-rekursiv:      go n day = go (n - 1) (tomorrow day)
--                       -> Ergebnis des Aufrufs wird direkt zurückgegeben
--
--   NICHT tail-rekursiv (z.B. capital aus Lektion 5):
--                       capital d i t = (capital d i (t - 1) + d) * (1 + i / 100)
--                       -> nach dem Aufruf kommen noch + d und * ... dazu,
--                          jede Ebene muss also auf die nächste warten
--
-- Das Zwischenergebnis (der aktuelle Tag) wird als Argument mitgegeben
-- (Akkumulator). Deshalb muss sich keine Ebene etwas merken, und der
-- Compiler kann die Rekursion intern wie eine Schleife ausführen
-- (kein wachsender Aufruf-Stack).
--
-- mod n 7: nach 7 Tagen ist wieder derselbe Wochentag, also reichen
-- höchstens 6 Schritte (futureDay 1000 braucht so nur 6 statt 1000 Aufrufe).
-- Nebeneffekt: mod (-1) 7 = 6, negative n gehen also rückwärts.
--
-- ghci> futureDay 10 Monday
-- Thursday
-- ghci> futureDay (-1) Monday
-- Sunday

futureDay :: Int -> Weekday -> Weekday
futureDay n day = go (n `mod` 7) day
  where
    go :: Int -> Weekday -> Weekday
    go 0 d = d
    go k d = go (k - 1) (tomorrow d)

-- ************* AUFGABE 6-11 *************
-- Dieselbe Funktion mit iterate.
--
-- iterate f x erzeugt die UNENDLICHE Liste
--   [x, f x, f (f x), f (f (f x)), ...]
-- also: Startwert, dann f einmal angewendet, dann zweimal, usw.
--
--   iterate tomorrow Monday = [Monday, Tuesday, Wednesday, ...]  (endlos weiter)
--   iterate (*2) 1          = [1, 2, 4, 8, 16, ...]
--
-- Mit !! k holt man das Element an Position k, also "f k-mal angewendet":
--   iterate tomorrow Monday !! 3 = Thursday
--
-- Warum stürzt eine unendliche Liste nicht ab? -> Lazy Evaluation:
--   Haskell berechnet einen Wert erst, wenn er wirklich gebraucht wird.
--   !! 3 braucht nur die ersten 4 Elemente, also werden nur diese berechnet,
--   der Rest der Liste existiert nie wirklich.
--
--   Strict Evaluation (z.B. JavaScript, Java): Argumente werden VOR dem
--   Funktionsaufruf vollständig ausgerechnet. Dort würde iterate versuchen,
--   zuerst die ganze unendliche Liste zu bauen -> endet nie.
--
-- Weitere Beispiele für lazy evaluation:
--   ghci> take 3 (iterate (*2) 1)
--   [1,2,4]
--   ghci> take 5 [1 ..]          -- [1 ..] ist auch unendlich
--   [1,2,3,4,5]
--
-- ghci> futureDayIterate 10 Monday
-- Thursday

futureDayIterate :: Int -> Weekday -> Weekday
futureDayIterate n day = iterate tomorrow day !! (n `mod` 7)
