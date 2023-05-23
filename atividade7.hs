-- IDENTIFICAÇÃO

atividade = 7

nome = "Lucas de Araújo Torres"

matricula = "557156"



-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio


intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs



instance Show Poly where
  show (Poly coef) = intercalate " + " (terms coef)
    where
      terms [] = ["0"]
      terms xs = reverse $ zipWith term [0..] (reverse xs)
      term 0 c = show c
      term 1 c = showCoefficient c ++ "x"
      term n c = showCoefficient c ++ "x^" ++ show n
      showCoefficient c
        | c == 1 = ""
        | c == -1 = "-"
        | otherwise = show c




-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly coef) x = foldr (\c acc -> c + x * acc) 0 coef


-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0
