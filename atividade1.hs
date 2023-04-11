

modificaLista :: [a] -> [a]
modificaLista l = take (length l - 2) (drop 1 l)

--funçao modificalista recebe uma lista e retorna sem o primeiro e ultimo elemento
--fazemos como argumentos de take o indice de ate onde queremos pegar a lista e a funcao drop, que removera o primeiro elemento da lista

