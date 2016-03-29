-- 18/03/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--

suma x y = x + y
doble x = 2* x
normaVectorial v1 v2 = sqrt (v1^2 + v2^2)
funcionConstante8 x = 8
respuestaATodo = 42
-- Primera parte todo piola ! 

unoSiCero n | n == 0 = 1
			| otherwise= 0

-- Medio compliqueti la sintaxis jajaj

signo n | n > 0 = 1
		| n == 0 = 0
		| n < 0 = (-1)

-- costo pero salio

valorAbsoluto x = sqrt(x^2)

maximo2 v1 v2 | v1 > v2 = v1 
			  | v1 < v2 = v2
			  | v1 == v2 = v1

-- Y ahora ...

maximo3 v1 v2 v3 = maximum [v1,v2,v3]
-- Es trampa ??

esPositivo n | n >= 0 = True
		   | otherwise = False

esPar n | even n = True
		| otherwise = False

esPar1 x = (div x 2) * 2 == x
-- Div es el cosiente entero

esPar2 x = (mod x 2) == 0
-- mod es el cociente

