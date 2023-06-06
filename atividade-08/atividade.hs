-- MÓDULOS

import System.IO
import System.Environment (getArgs)
-- não import mais nada!

--IDENTIFICAÇÃO

atividade = 8
matricula = "557156"
nome      = "Lucas de Araújo Torres"

-- ATIVIDADE

-- Construir programa que leia
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa deve susturuir
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações deve ser
-- a saída. Seu nome precisa se o de f
-- o arquivo de saída deter nome igual
-- com prefixo "subst-". .

-- MATENHA O .hs COM NOME
-- "atividade.hs" E CONSEQUENTEMENTE
-- EXECUTÁVEL COMO SENDO
-- "atividade08".

-- CÓDIGO



main = do
  args <- getArgs
  case args of
    [filename, w1, w2] -> do
      contents <- readFile filename
      let modifiedContents = replaceWord w1 w2 contents
          outputFilename = "subst-" ++ filename
      writeFile outputFilename modifiedContents
      putStrLn $ "Arquivo modificado criado: " ++ outputFilename
    _ -> putStrLn "Uso: atividade-08 <arquivo> <palavra1> <palavra2>"

replaceWord :: String -> String -> String -> String
replaceWord w1 w2 = unwords . map replace . words
  where
    replace word
      | word == w1 = w2
      | otherwise = word


-- INFORMAÇÕES

-- Compilação e execução

-- $ ghci atividade-08.hs
-- $ ./atividade-08 historia.txt Pedro Pablo

-- Onde "historia.txt" é um arquivo de texto
-- em que toda palavra "Pedro" é substituída
-- por "Pablo".

-- Exemplo

-- "historia.txt" de entrada,

-- Pedro vivia numa casa de pedra.
-- Mas Pdro queria morar numa 
-- casa de ouro. Pobre Pedro!

-- "subst-historia.txt" criado,

-- Peblo vivia numa casa de pedra.
-- Mas Pabloqueria morar numa 
-- casa de ouro. Pobre Pablo!
-- 
