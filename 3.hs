-- 12/04/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--

factorial n | n == 0 = 1
			| n > 0 = n* factorial(n-1)
			| otherwise = error "GIl"


fibonacci n | n == 0 = 0
			| n == 1 = 1
			| n >= 2 = fibonacci(n-1) + fibonacci (n-2)
			| otherwise = error "TOGA"

par :: Integer -> Bool

par n | n == 1 = False 
	  | n == 0 = True
	  | n > 2 = par (n-2)
      | n < 0 = par (n+2)
	  | otherwise = error "Michifu"



esimoImpar n | n == 1 = 1
             | n >1 = esimoImpar (n-1) + 2
             | otherwise = error "Te pintaron estrellitas en el aire"


sumaImpar n | n == 1 = 1 
            | n > 1 =  sumaImpar(n-1) + esimoImpar (n)
            | otherwise = error "GIl"

dobleFact :: Integer -> Integer
dobleFact n | n  == 0 = 1
            | n > 0 = n * dobleFact (n-2)


noTermina n | n >= 0 = True 
            | n < 0 = noTermina(-1)