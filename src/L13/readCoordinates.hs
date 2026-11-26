import Text.Read (readMaybe)
import Vector

-- ************* AUFGABE 7-1 *************
-- String mit Zahlen -> Liste von Vektoren, oder eine Fehlermeldung.
--   Left  "..."  = Fehler (mit Meldung)
--   Right [...]  = alles ok (mit dem Ergebnis)

readCoordinates :: String -> Either String [Vector]
readCoordinates input =
  case parseNumbers (words input) of
    Left err -> Left err
    Right numbers -> toVectors numbers

-- Teilaufgabe 1: jedes Wort in eine Zahl umwandeln.
-- readMaybe liefert Just zahl, wenn es klappt, sonst Nothing.
-- Gibt es ein einziges Nothing, ist das Ganze ein Fehler (Fehlerart 1).
parseNumbers :: [String] -> Either String [Double]
parseNumbers ws
  | null invalid = Right [x | Just x <- parsed]
  | otherwise = Left ("Keine gueltigen Zahlen: " ++ unwords invalid)
  where
    parsed = map readMaybe ws :: [Maybe Double]
    invalid = [w | (w, Nothing) <- zip ws parsed]

-- Teilaufgabe 2: Zahlen paarweise zu Vektoren zusammensetzen.
-- Ungerade Anzahl = ein x ohne y (Fehlerart 2).
toVectors :: [Double] -> Either String [Vector]
toVectors nums
  | odd (length nums) = Left ("Ungerade Anzahl Werte (" ++ show (length nums) ++ "), keine vollstaendigen Paare")
  | otherwise = Right (pairUp nums)

-- (x, y), (x, y), ... -> Vector x y : ...
pairUp :: [Double] -> [Vector]
pairUp (x : y : rest) = Vector x y : pairUp rest
pairUp _ = []
