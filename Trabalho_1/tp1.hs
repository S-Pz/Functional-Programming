import Data.Maybe (fromMaybe)

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
  [ ("AMENDOIM", 3.99)
  , ("HEINKEN 5L", 64.99)
  , ("AGUA MINERAL", 2.50)
  , ("CHOCOLATE", 3.99)
  , ("LEITE", 2.99)
  , ("Acucar", 3.0)
  , ("Cafe", 6.5)
  , ("Oleo", 4.7)
  , ("Sal", 1.5)
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
valor $$ n = inteiroStr ++ "." ++ decimalStr'
  where
    inteiro = truncate valor
    decimal = truncate ((valor - fromIntegral inteiro) *10 ^ n)
    inteiroStr = show inteiro
    decimalStr = show decimal
    decimalStr' = if length decimalStr < n then repete '0' (n - length decimalStr) ++ decimalStr else decimalStr
    

dinheiro :: Valor -> [Char]
dinheiro valor = "$" ++ (valor $$ 2)

-- Questão 06 - Crie uma função chamada formataItem que recebe um Item e retorna um item
-- da nota fiscal em uma String de 80 caracteres. Essa função deve formatar a String retornada
-- utilizando espaços em banco da seguinte forma: 45 caracteres para o nome do produto; 25 para o
-- valor e a quantidade; e 10 para o subtotal.

formataItem :: Item -> [Char]
formataItem (produto, quantidade) = nome ++ valor ++ subtotal 
  where
    nome = alinhaEsq (fst produto) '.' 45
    valor = alinhaDir (dinheiro (snd produto) ++ " x " ++ show quantidade ++ " = ") '.' 25
    subtotal = alinhaDir (dinheiro (fromIntegral quantidade * snd produto)) ' ' 10

-- Questão 07 - Crie uma função chamada total que recebe como entrada uma lista de itens,
-- calcula o valor total dos itens da lista e retorna esse valor formatado como dinheiro. 
-- O valor total é igual à soma de dos valores dos produtos dos itens de venda multiplicados por sua quantidade
-- vendida.

-- item = (produto, quantidade)
  -- produto = (nome, valor)
total :: [Item] -> [Char]
total lista =  dinheiro total1
  where
    total1 = sum [fromIntegral quantidade * valor | ((_,valor),quantidade) <- lista]

-- Questão 08 Crie uma função chamada notafiscal que recebe como entrada uma lista de
-- itens e retorna uma String que representa a nota fiscal da venda. A largura da linha da nota é 80
-- caracteres.

notafiscal :: [Item] -> IO()
notafiscal lista = putStr ("\n" ++ repete '*' 80 ++ "\n"
                            ++ cabecalho ++ "\n"
                            ++ repete '*' 80 ++ "\n\n"
                            ++ itens 
                            ++ repete '*' 80 ++ "\n"
                            ++ total2
                            ++ repete '*' 80 ++ "\n")
  where
    cabecalho = alinhaDir "NOTA FISCAL" ' ' 45
    itens = concat [formataItem item ++ "\n" | item <- lista]
    total2 = alinhaEsq "TOTAL" '.' 45 ++ total lista ++ "\n"


-- Questao 09 Crie a função proditem que não possui entradas e retorna uma lista de Item contendo todos os
-- elementos da lista produtos e a quantidade sendo o índice de cada produto na lista.
-- Crie também a função proditemx, que é semelhante a função proditem, porém recebe como entrada as uma
-- lista de quantidades.
-- Se esta lista de quantidades for menor que a lista de produtos, então somente os n primeiros produtos são
-- considerados, onde n é o tamanho da lista de quantidades.

prodItem :: [Item]
prodItem = [(x, prodItemaux x) | x <- produtos]
 
prodItemaux :: Produto -> Int
prodItemaux x = 1 + fromMaybe 0 (index x produtos)

proditemx :: [Quantidade] -> [Item]
proditemx lista_q = [(x, lista_q !! fromMaybe 0 (index x produtos)) | x <- produtos, fromMaybe 0 (index x produtos) < (length lista_q)]


-- Questao 10 Crie a função chamada itensn que recebe uma lista da tupla(Nome,Quantidade) e retorna uma lista do tipo 
-- Item, com os produtos identificados a partir do nome na lista produtos (Questão 1).  

-- Crie também a função chamada itensi que recebe uma lista da tupla(Int, Quantidade) e retorna uma lista do tipo Item, sendo que  
-- o Int da tupla corresponde ao índice na lista produtos.

itensn :: [(Nome, Quantidade)] -> [Item]
itensn lista = [(p, q) | (n, q) <- lista, p <- produtos, fst p == n]


itensi :: [(Int, Quantidade)] -> [Item]
itensi lista = [(produtos !! i, q) | (i, q) <- lista]

