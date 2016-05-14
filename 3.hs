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
             | n > 1 = esimoImpar (n-1) + 2
             | otherwise = error "Te pintaron estrellitas en el aire"


sumaNImpar n | n == 1 = 1 
             | n > 1 =  sumaNImpar(n-1) + esimoImpar (n)
             | otherwise = error "GIl"

dobleFact :: Integer -> Integer
dobleFact n | n  == 0 = 1
            | n > 0 = n * dobleFact (n-2)


noTermina n | n >= 0 = True 
            | n < 0 = noTermina(-1)

mul3 :: Integer -> Bool
mul3 n | n == 1 = False
       | n == 2 = False
       | n == 0 = True
       | n < 0 = mul3 (-n)
       | n > 0 = mul3 (n-3)
       | otherwise = error "Mal ahi"

sumaImparsCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparsCuyoCuadSeaMenorQue n | n < 0 = error "Son negativos papa"
                                | n > 0 = sumaImpares(ultimoImpar (ultimoN n 1))

ultimoN :: Integer -> Integer -> Integer
ultimoN n b | n > b^2 = ultimoN n (b+1)
            | n == b^2 = b
            | n < b^2 = (b-1)

ultimoImpar :: Integer -> Integer
ultimoImpar n | mod n 2 == 0 = n-1 
              | mod n 2 /= 0 = n

sumaImpares n | n == 1 = 1
              | n < 1 = error "Manqueaste codigo"
              | n > 1 =  n + sumaImpares (n-2)