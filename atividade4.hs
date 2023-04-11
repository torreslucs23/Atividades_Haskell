-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "557156" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Lucas de Araújo Torres" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Implementar função que receba uma lista
-- ou string de entrada e retorne uma outra 
-- equivalente sem repetiições de elementos,



unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/=x) xs)  
-- caso base e a lista vazia
--aqui juntamos sempre o primeiro elemento com a recursao do resto da lista, ja filtrando o elemento
--a funcao de filter vai pegar todos os elementos restante que sejam diferentes de x. Ela tem como parametros a condicao e a lista



-- Exemplos:

-- >> unique "a1abaa1123b"
-- "ab23"
-- >> unique [2,1,1,3,3,1,1,3,2
-- [2,1,3]]

-- Obs: (1) Note que a ordem relativa das chaves
-- remanescentes se preserva. (2) Se existir uma função em
-- Haskell que faça a mesma coisa, não deve ser usada. 


-- 2

-- Construa função que remova o valor mínimo de uma lista.  

takeMin :: (Ord a) => [a] -> a
takeMin (x:xs) = foldl (\acc n -> if n < acc then n else acc) x xs 

--implementei essa funçao para pegar o elemento mínimo de uma lista
--para implementar ela, usei filter, que em haskell e foldl

delete'min :: Ord a => [a] -> [a]
delete'min l = let minimo = takeMin l
         in remove l minimo
--funcao principal que pega o minimo de uma lista e chama a funcao remove que formara a nova lista sem o elemento minimo

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove [x] minimo = if x == minimo then [] else [x]
remove (x:xs) minimo = if x == minimo then xs else x : remove xs minimo

--funcao que remove o numero minimo de fato. Note que se x=minimo, entao eu retorno so a cauda da lista que esta na funcao

--note que mantive a funcao delete'min de acordo com que a questao pediu. Basta chama-la e passar uma lista como parametro



-- Exemplos,

-- >> delete'min [1,3,2,5]
-- [3,2,5]
-- >> delete'min [7,3,2,5,6]
-- [7,3, 5,6]

-- Obs: (1) Se o valor mínimo se repetir
-- então somente a primeira aparição deve 
-- ser removida. (2) Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não deve se utilizada