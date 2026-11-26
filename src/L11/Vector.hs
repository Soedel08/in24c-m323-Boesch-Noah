{-# LANGUAGE GADTs #-}

module Vector where

-- 2D-Vektor bzw. Punkt im Koordinatensystem
-- 1. Feld: x-Koordinate, 2. Feld: y-Koordinate
data Vector where
  Vector :: Double -> Double -> Vector
  deriving (Show, Eq)
