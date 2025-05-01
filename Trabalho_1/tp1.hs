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
index a b = indexAuxiliar a b 0  
  where
    indexAuxiliar _ [] _ = Nothing
    indexAuxiliar y (z:zs) i
      | y == z = Just i
      | otherwise = indexAuxiliar y zs (i + 1)

elemento :: [a] -> Int -> Maybe a
elemento xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise = Just (xs !! n)

-- Questão 03 Crie a função:
-- addProduto que recebe uma lista de Produto e um Produto e retorna a lista com o produto adicionado no final.
-- Crie a função remProduto que recebe uma lista de Produto e um Nome e retorna a lista sem o produto com nome passado. 
-- Crie a função buscaProduto que recebe uma lista de Produto e um Nome e retorna um Maybe Produto, que será
-- o produto da lista possui aquele nome ou Nothing (ver tipo Maybe).

addProduto :: [Produto] -> Produto -> [Produto]
addProduto lista produto = lista ++ [produto]

remProduto :: [Produto] -> Nome -> [Produto]
remProduto lista nome = [produto | produto <- lista, fst produto /= nome]

buscaProduto :: [Produto] -> Nome -> Maybe Produto
buscaProduto lista nome = buscaAuxiliar lista nome
  where
    buscaAuxiliar [] _ = Nothing
    buscaAuxiliar (produto:b) nome
      | fst produto == nome = Just produto
      | otherwise = buscaAuxiliar b nome

-- Questão 04 - Crie uma função chamada alinhaEsq que recebe uma String, um caractere e
-- um inteiro como parâmetros e retorna uma String composta pela String informada e uma
-- sequência do caractere de modo que o tamanho total da String resultante ocupe quantidade
-- informada. 
-- Crie uma função chamada alinhaDir que recebe uma String, um caractere e um inteiro
-- como parâmetros e retorna uma String composta por uma sequência do caractere de modo
-- seguida da String informada e que o tamanho total da String resultante ocupe quantidade
-- informada.

alinhaEsq :: [Char] -> Char -> Int -> [Char]
alinhaEsq str c n = str ++ repete c (n - length str)

alinhaDir :: [Char] -> Char -> Int -> [Char]
alinhaDir str c n = repete c (n - length str) ++ str

-- Questão 05 - Crie um operador $$ não associativo e com prioridade 5. Esse operador recebe
-- um Valor e um inteiro e retorna uma String com o valor no formato [0-9]+. [0-9]1 [0-9]2 ...[0-9]n. 
-- Crie também uma função chamada dinheiro que recebe um Valor e usa o operador $$ para transformar
-- esse valor em uma String com um cifrão ($) na frente e sempre dois algarismos depois do ponto.

($$) :: Valor -> Int -> [Char]
valor $$ n = 
    let inteiro = floor valor
        decimal = (valor - fromIntegral inteiro) *10 ^ n
        inteiroStr = show inteiro
        decimalStr = show decimal
        decimalStr' = if length decimalStr < n then repete '0' (n - length decimalStr) ++ decimalStr else decimalStr
    in inteiroStr ++ "." ++ decimalStr'

dinheiro :: Valor -> [Char]
dinheiro valor = "$" ++ (valor $$ 2)