module Biblioteca.Util where
    -- Criar o where para as funções repete e alinhaEsq, só para facilitar as expostações
    -- Para a prova Eq, Show e ord <- gravar pq cai na prova de Programação funcional 
    data Data = Data {
        dia:: Int,
        mes:: Int,
        ano:: Int
    } deriving (Eq, Show)

    dataStr:: Data -> String
    dataStr (Data d m a) = formata d ++ "/" ++ formata m ++ "/" ++ show a
        where
            formata n = if n < 10 
                then 
                    '0' : show n
                else
                    show n

    repete :: a -> Int -> [a]
    repete elemento n
        | n <= 0 = []
        | otherwise = elemento : repete elemento (n - 1)

    alinhaEsq :: String -> Char -> Int -> String
    alinhaEsq str c n = str ++ repete c (n - length str)

    alinhaDir :: String -> Char -> Int -> String
    alinhaDir str c n = repete c (n - length str) ++ str

    formata:: String -> String -> String
    formata [] [] = []
    formata chave valor = alinhaEsq chave '.' 30 ++ ":" ++ alinhaDir valor ' ' 50
    
    -- Divide uma string em uma lista de strings com base em um caractere delimitador.
    splitPor :: Char -> String -> [String]
    splitPor _ [] = [""]
    splitPor delimitador (c:cs)
        | c == delimitador = "" : resto
        | otherwise        = (c : head resto) : tail resto
            where
                resto = splitPor delimitador cs

    -- Retorna o elemento em uma posição específica de uma lista.
    elementoNaPosicao :: Int -> [String] -> String
    elementoNaPosicao 0 (x:_)  = x
    elementoNaPosicao n (_:xs) = elementoNaPosicao (n - 1) xs
    elementoNaPosicao _ []     = error "Índice fora da faixa"

    -- Extrai um campo específico de uma string que é dividida por um delimitador.
    extrairCampo :: Char -> Int -> String -> String
    extrairCampo delimitador n linha =
        let partes = splitPor delimitador linha
        in elementoNaPosicao n partes

    -- Une uma lista de strings em uma única string, separadas por um caractere.
    joinPor :: Char -> [String] -> String
    joinPor _ [] = ""
    joinPor _ [x] = x
    joinPor sep (x:xs) = x ++ [sep] ++ joinPor sep xs

    dividirPorNovaLinha :: String -> [String]
    dividirPorNovaLinha = splitPor '\n'

    qualquer :: (a -> Bool) -> [a] -> Bool
    qualquer _ [] = False
    qualquer p (x:xs)
        | p x       = True
        | otherwise = qualquer p xs

    juntarComNovaLinha :: [String] -> String
    juntarComNovaLinha [] = ""
    juntarComNovaLinha xs = concat [s ++ "\n" | s <- xs]
      where
        concat [] = ""
        concat (str:strs) = str ++ concat strs