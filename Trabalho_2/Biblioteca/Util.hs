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
    