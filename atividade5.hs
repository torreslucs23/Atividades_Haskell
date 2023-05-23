
-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "557156" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Lucas de Araújo Torres" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Construa função que 
-- receba uma string e 
-- retorne a lista das 
-- tuplas das frequencias dos
-- seus caracteres
frequencias :: String -> [(Char, Int)]
frequencias xs = percorre xs []
  where percorre [] freqs = freqs
        percorre (c:cs) freqs = percorre cs (add c freqs)
        add c [] = [(c,1)]
        add c ((x,n):freqs) | c == x = (x,n+1):freqs
                            | otherwise = (x,n) : add c freqs
-- criei a função percorre para ir em cada elemento da string e adicionar na sua tupla. Caso ele nunca tenha ocorrido, eu adiciono na lista de freqs

-- Exemplos:

-- >> freq "abcdaadd"
-- [('a',3), ('b',1),('c',1),('d',3)]
-- >> freq "A casa"
-- [('A',1), ('a', 2), ('c',1), ('s', 1), (' ',1) ]

-- Se existir uma função em
-- Haskell que faça a mesma coisa, não use.

-- 2

-- Construa função que ordene
-- a lista de tuplas da questão
-- por valor de frequencia,

-- aqui eu recebi uma string e fiz a ocorrência dela, porém em ordem crescente, diferente da função frequencias

freqOrdenadas :: String -> [(Char, Int)]
freqOrdenadas xs = sort (frequencias xs)
  where sort [] = []
        sort (x:xs) = insert x (sort xs)
        insert x [] = [x]
        insert x (y:ys) | snd x <= snd y = x:y:ys
                        | otherwise = y : insert x ys

-- note que aqui eu crio uma função sort dentro de freqOrdenadas para fazer a ordenação de inserção
-- a função snd retorna o segundo elemento da tupla

-- Exemplos,

-- >> s = freqSort freq "aaaa22p"
-- [('p',1), ('2', 2), ('a', 4)]

-- Obs: Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não use.



