-- 29/03/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--


doble :: Integer -> Integer
doble x = x + x

cuadruple :: Integer -> Integer
cuadruple x = doble (doble x)

dist :: Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

dist1 :: (Float, Float) -> (Float, Float) -> Float
dist1 p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2))

crearPar :: Integer -> Integer -> (Integer, Integer)
