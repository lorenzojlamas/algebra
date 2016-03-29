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
-- Div es el cons
esPar2 x = (mod x 2) == 0

yLogico1 x y = x && y

yLogico True True = True
yLogico True False = False
yLogico False True  = False
yLogico False False  = False

yLogico2 True True = True
yLogico2 x y = False

yLogico3 False _ = False
yLogico3 _ y = y


f n1 n2 n3 | n2 < 10 = n1
		   | n2 >= 10 = n1 + n3


nand x y = not (x && y)

unaRaiz a b c = ( (-b) + sqrt(b^2 - 4*a*c) ) / (2*a)

unaRaiz1 a b c | (b^2 - 4*a*c) < 0 = error "No hay raices reales"
			   | (b^2 - 4*a*c) >= 0 = ( (-b) + sqrt(b^2 - 4*a*c) ) / (2*

pitagoras a b c = c^2 == a^2 + b^2
