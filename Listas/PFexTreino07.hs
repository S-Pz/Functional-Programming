-- 1. Crie uma função que solicita do usuário o raio de um círculo e imprime a área.
-- Afunção deve apresentar um erro se o usuário não digitar o raio. 
area :: Float -> Float
area raio = 
    |raio >= 0 = pi * raio^2
    |otherwise = error "Raio com valor negativo."

-- 2. Crie uma função que solicita do usuário uma String e verifica se a mesma é um palíndromo ou não. 
-- A função repete essa ação até que o usuário digite uma String vazia.
inverte :: [a] -> [a]
inverte [] = []
inverte (h:t) = inverte t ++ [h]

palindromo :: Eq a => [a] -> Bool
palindromo lista = lista == inverte lista

palindromoIO :: IO ()
palidromoIO = do
    putStr "Digite uma palavra:"
    palavra <- getLine
    
    if palavra == "" then return ()
    
    else do
        if palavra == palindromo palavra
            then putStrLn (palavra ++ "é palindromo")
        else putStrLn (palavra ++ "não é palindromo")


-- 3. Crie uma função que solicita do usuário diversos números até que o usuário decida parar. 
-- No final o programa imprime o maior, o menor e a média. 
-- Trate a possibilidade do usuário não digitar o número usando o tipo Maybe

-- 4. Crie uma função que mostra um menu em que o usuário pode escolher sair do programa,imprimir um quadrado ou imprimir um triângulo. 
-- Ao selecionar uma das figuras geométricas, o usuário deve informar o tamanho do lado e caractere que compõe a figura. 
-- Por exemplo: um triângulo de tamanho 4 e formado por caracteres ‘0’. 
-- Trate a possibilidade do usuário não digitar o tamanho ou o caractere usando o tipo Maybe. 

-- 0
-- 00
-- 000
-- 0000

-- 5. Crie o tipo algébrico Pessoa com nome (String) e idade (Int).
-- Crie uma função que pergunta ao usuário se ele deseja cadastrar uma pessoa.
-- Se ele responder ‘S’ ou ‘s’, ele deve informar o nome e a idade da pessoa.
-- Essa ação se repete até o quando o usuário digitar ‘N’ ou‘n’.
-- Após isso, a função deve imprimir os dados de cada pessoa e ordenados por ordem alfabética do nome conforme o seguinte alinhamento:

-- Nome Idade
-- Andre Teixeira 40
-- Maria da Silva 28
-- Zuleica Santos 55