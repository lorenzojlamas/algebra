-- 10/04/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--


potencia :: Float -> Integer -> Float
potencia a n | n == 0 = 1
			 | n == 1 = a
			 | n > 1 = a * potencia a (n-1) 

division :: Integer -> Integer -> (Integer,Integer) --qr
division a d | a >= d = (fst qr + 1,snd qr )
			 | a < d = (0,a)
   where qr = division (a-d) d


esPrimo :: Integer -> Bool

esPrimo n | n > 1 = auxEsPrimo  n 2
		  | n == 1 = error "Uno no es ni primo ni compuesto"
		  | n < 1 = error "Dale pa, posta ?"

auxEsPrimo n k | (snd (division n k) == 0) && (n == k ) = True   
			   | (snd (division n k) == 0) && (n /= k ) = False
			   | (snd (division n k) /= 0) && (n /= k ) = auxEsPrimo n (k+1)


suma :: [Float] -> Float
suma lista | length lista == 0 = 0
		   | otherwise = head lista + suma (tail lista)

productoria :: [Integer] -> Integer

productoria xs | length xs == 0 = 0
 			   | otherwise = productoriaAux xs

productoriaAux xs | length xs == 0 = 1
			      | otherwise = head xs * productoriaAux (tail xs)


reverso :: [a] -> [a]

reverso [] = []
reverso xs = (reverso (tail xs)) ++ [head xs]


capicua :: [Integer] -> Bool

capicua xs | xs == [] = True
		   | xs == (reverso (tail xs)) ++ [head xs] = True
		   | otherwise = False

sumaV :: [Integer] -> [Integer] -> [Integer]

sumaV xs ys | (xs == [] || ys == []) = [] 
			| not (xs == [] || ys == []) = [head xs + head ys] ++ (sumaV (tail xs) (tail ys))


producIn :: [Float] -> [Float] -> Float

producIn a b | length a == 0 = 0
             | length a == 1 = head a * head b
			 | otherwise = head a * head b + producIn (tail a) (tail b)


productoInterno a b | length a /= length b = error "No son iguales papa"
					| length a == 1 = head a * head b 
					| a /= [] = (head a * head b) + productoInterno (tail a) (tail b) 



noTieneDivisoresHasta :: Integer -> Integer -> Bool

noTieneDivisoresHasta k n | k >= n = False
						  | (snd(division n k) == 0 && k ==1) = True 
						  | (snd(division n k) == 0 && k /=1) = False 
						  | (snd(division n k) /= 0 && k /=1) = noTieneDivisoresHasta (k-1) n


-- n = 35
-- k = 5 
-- 