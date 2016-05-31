-- 24/03/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--

mcd :: Integer -> Integer -> Integer
mcd a b | b == 0 = a 
		| otherwise = mcd b (mod a b)

fact :: Integer -> Integer

fact 0 = 1
fact n = (n * fact (n - 1))

enIngles :: Integer -> [Char]

enIngles 1 = "one!"
enIngles 2 = "two"
enIngles 3 = "three"
enIngles 4 = "four"
enIngles 5 = "five"
enIngles 6 = "six"
enIngles x = "not sure :s"

sumarVectores :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sumarVectores (x1 , y1) (x2 , y2) = (x1 + x2 , y1 + y2)

--cabeza :: [Integer] -> Integer
cabeza (x:xs) = x

first :: (a, b, c) -> a
first (x, _, _) = x 

longitud :: [a] -> Integer

longitud [] = 0
longitud (y:xs) = 1 + longitud xs

--iniciales :: [Char] -> [Char] -> [Char]
-- Haer iniciales


type Racional = (Integer, Integer)

suma :: Racional -> Racional -> Racional

suma (a,b) (c,d) = (a*d + b *c , b*d)


type Punto = (Integer, Integer)

--dist :: Punto -> Punto -> Float
-- Arrancamos a jugar con tipos de data que no estan definidos, este conjunto
-- se puede mosttrar, comparar y esta ordenado

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show, Eq, Ord, Enum)

esFinde :: Dia -> Bool

esFinde Sabado = True
esFinde Domingo = True
esFinde _ = False

diaHabil :: Dia -> Bool

diaHabil a = not(esFinde a)

soloAlgebra :: [Dia] -> [Dia]

soloAlgebra xs | xs == [] = [] 
			   |(head xs == Martes) || (head xs == Viernes) = head xs : soloAlgebra (tail xs)
			   | otherwise = soloAlgebra (tail xs)


tuplas :: [a] -> [b] -> [(a,b)]

tuplas a b | (length a == 0) || (length b == 0) = []
		   | otherwise = (head a, head b) : tuplas (tail a) (tail b) 

potencia :: Racional -> Integer -> Racional

potencia a b = a
