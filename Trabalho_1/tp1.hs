-- Questão 01: Crie os tipos: Nome, equivalente a String; Valor, equivalente a Float;
-- Quantidade, equivalente a Int; Produto, com nome e valor; Item, com produto e quantidade.
-- Crie uma função chamada produtos que retorna uma lista com 10 produtos a sua escolha.

type Nome = String
type Valor = Float
type Quantidade = Int
type Produto = (Nome, Valor)
type Item = (Produto, Quantidade)

produtos :: [Produto]
produtos = 
  [ ("Arroz", 5.0)
  , ("Feijão", 4.5)
  , ("Macarrão", 3.2)
  , ("Farinha", 2.8)
  , ("Açúcar", 3.0)
  , ("Café", 6.5)
  , ("Óleo", 4.7)
  , ("Sal", 1.5)
  , ("Leite", 4.0)
  , ("Biscoito", 2.2)
  ]

-- Questão 02  Crie as seguintes funções auxiliares: 
-- 1 - Repete, que recebe um elemento de qualquer tipo e um inteiro e retorna uma lista com o elemento repetido o número de vezes
-- informado;
-- 2 - Index, que recebe um elemento de um subtipo de Eq e uma lista do mesmo tipo e 
-- retorna o índice do elemento na lista se ele existir ou Nothing, caso o elemento não exista na lista;
-- 3 - Elemento que recebe uma lista de qualquer tipo e um índice (Int) e retorna o elemento no índice
-- informado ou Nothing, para um índice inválido. Para essa questão pesquise o tipo Maybe do
-- Haskell.

repete :: a -> Int -> [a]
-- repete elemento n = [elemento | _ <- [1..n]]
-- ou 
repete elemento n = 
    if n <= 0 then []
    else elemento : repete elemento (n - 1)

index :: Eq a => a -> [a] -> Maybe Int
index _ [] = Nothing
index x xs = indexHelper x xs 0
  where
    indexHelper _ [] _ = Nothing
    indexHelper y (z:zs) i
      | y == z = Just i
      | otherwise = indexHelper y zs (i + 1)

elemento :: [a] -> Int -> Maybe a
elemento xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise = Just (xs !! n)