-- 18/03/16
-- Lorenzo Jose Lamas
-- Algebra I
-- Labo
--


data Direccion = Norte | Sur | Este | Oeste deriving (Show, Eq, Ord, Enum)
type Tortuga = (Pos, Direccion)
type Pos = (Integer, Integer)

arranca :: Tortuga

arranca = ((0,0) , Norte)

girarDerecha (x,Norte) = (x,Este)
girarDerecha (x,Este) = (x,Sur)
girarDerecha (x,Sur) = (x,Oeste)
girarDerecha (x,Oeste) = (x,Norte)

avanzar :: Tortuga -> Integer -> Tortuga

avanzar ((a,b), Norte) x = ((a,b+x), Norte) 

data Figura = Rectangulo (Float,Float) (Float,Float) | Circulo (Float,Float) Float deriving (Show)

c1 = Circulo (0,0) pi

r1 :: Float -> Figura

r1 x = Rectangulo (0,0) (sqrt((x^2)/2),sqrt((x^2)/2))

--area :: Figura -> Float

--area Circulo (a,b) c = areaCirculo (a,b) c
--area Rectangulo (a,b) (c,d) = areaRectangulo (a,b) (c,d) 

--areaCirculo (_,_) b = pi * (b^2) 

--areaRectangulo (a,b) (c,d) = b

-- hacer interseccion

data ProgAritmetica = Vacio | CongruentesA Integer Integer

instance Show ProgAritmetica where
	show Vacio = "{}"
	show (CongruentesA x d) = "{a en z | a = " ++ show x ++ "(mod " ++ show d ++ ")}"

suma :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica

suma Vacio _ = Vacio
suma _ Vacio = Vacio
--suma (CongruentesA a b ) (CongruentesA c d) = CongruentesA (a + c) mcd (b-d)

iguales ProgAritmetica -> ProgAritmetica -> Bool

iguales Vacio Vacio = True
iguales Vacio _ = False
iguales _ Vacio = False
iguales (CongruentesA a b) (CongruentesA c d) = (b1 == b2) && (mod a c) == (b d)


instance Eq ProgAritmetica where
	a == b = iguales a b 

