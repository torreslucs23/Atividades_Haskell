-- IDENTIFICAÇÃO
matricula = "557156" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Lucas de Araújo Torres" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeitos números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [y | y <- [2..x], x `mod` y == 0] 

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = length (divisores x) == 1 

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = [y | y <- [1..n], eprimo(y)] 
