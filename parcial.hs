-- 16/05/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--


esMultiploDe :: Integer -> Integer -> Bool

esMultiploDe a b | a > b = False
				 | otherwise = aux a a b

aux r a b | (r==a) && (a==b) = True
		  | a > b = False
		  | (r==a) && (a/=b) = aux r (a+r) b
		  | (r/=a) && (a==b) = True
		  | (r/=a) && (a/=b) = aux r (a+r) b


intercalar :: [Integer] -> [Integer] -> [Integer]

intercalar a b | a == [] = []
			   | a /= [] = head a: head b: intercalar (tail a) (tail b)
 

quitarTodosLos :: Integer -> [Integer] -> [Integer]

quitarTodosLos x xs = auxQTL x xs []

auxQTL x xs ys | x == head xs = auxQTL x (tail xs) ys
			   | x /= head xs = auxQTL x (tail xs) (x:ys)
			   | x == [] = ys