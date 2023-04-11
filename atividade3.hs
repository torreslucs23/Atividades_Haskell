-- IDENTIFICAÇÃO
matricula = "557156" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Lucas de Araújo Torres" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse-- implemente aqui

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = let (s1, s2) = break (== ' ') (strip xs) in (s1, dropWhile (== ' ') s2)

-- break separa a string em dois quando o caracter espaco for encontrado
-- usei a funcao strip para remover espacos vazios no comeco e no final da string
-- dropwhile servirá para encontrar os espaços posteriores que não foram apagados


-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr [] = []
splitStr xs = 
    let (word, rest) = popWord xs
    in
        if null word
        then splitStr rest
        else word : splitStr rest

-- aqui criei uma recursao onde no primeiro let eu retiro a primeira palavra e vou tratando o resto das palavras de forma recursiva
-- a primeira condicao checa se a palavra e nula, retornando apenas o resto, pois nao quero salvar valores nulos na minha lista final
-- o else concatena a lista atual com a que sera chamada recursivamente