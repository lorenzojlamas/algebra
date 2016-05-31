-- 16/05/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--

pertenece :: Integer -> [Integer] -> Bool

pertenece a xs | xs == [] = False
			   | a == head xs = True
			   | a /= head xs = pertenece a (tail xs)


hayRepetidos :: [Integer] -> Bool

hayRepetidos xs | length xs < 2 = False
				| head xs == head (tail xs) = True
				| head xs /= head (tail xs) = hayRepetidos (tail xs)




