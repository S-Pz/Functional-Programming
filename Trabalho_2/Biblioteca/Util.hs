module Biblioteca.Util where
     
    data Data = Data {
        dia:: Int,
        mes:: Int,
        ano:: Int
    } deriving (Eq, Show)

    dataStr :: Data -> String
    dataStr (Data d m a) = formataData d ++ "/" ++ formataData m ++ "/" ++ show a
        where
            formataData n = if n < 10 
                then 
                    '0' : show n
                else
                    show n

    formata :: String -> String -> String
    formata [] [] = []
    formata chave valor = alinhaEsq chave '.' 30 ++ ":" ++ alinhaDir valor ' ' 50
        where
            repete :: a -> Int -> [a]
            repete elemento n
                | n <= 0    = []
                | otherwise = elemento : repete elemento (n - 1)

            alinhaEsq :: String -> Char -> Int -> String
            alinhaEsq str c n = str ++ repete c (n - length str)

            alinhaDir :: String -> Char -> Int -> String
            alinhaDir str c n = repete c (n - length str) ++ str
    
    -- Divide uma string em uma lista de strings com base em um caractere delimitador.
    splitPor :: Char -> String -> [String]
    splitPor _ [] = [""]
    splitPor delimitador (c:cs)
        | c == delimitador = "" : resto
        | otherwise        = (c : head resto) : tail resto
            where
                resto = splitPor delimitador cs

    parseData :: String -> Data
    parseData str = Data (read d) (read m) (read a)
        where
            [d, m, a] = splitPor '/' str

    -- Extrai um campo específico de uma string que é dividida por um delimitador.
    extrairCampo :: Char -> Int -> String -> String
    extrairCampo delimitador n linha  = partes !! n
        where
            partes = splitPor delimitador linha
         
    -- Une uma lista de strings em uma única string, separadas por um caractere.
    joinPor :: Char -> [String] -> String
    joinPor _ [] = ""
    joinPor _ [x] = x
    joinPor sep (x:xs) = x ++ [sep] ++ joinPor sep xs

    dividirPorNovaLinha :: String -> [String]
    dividirPorNovaLinha = splitPor '\n'

    -- Verifica se pelo menos um elemento de uma lista satisfaz uma condição.
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

    showMainMenu :: IO (String)
    showMainMenu = do
        putStrLn "=========================================="
        putStrLn "           MENU PRINCIPAL"
        putStrLn "------------------------------------------"
        putStrLn "SELECIONE O MENU DESEJADO:"
        putStrLn "Alunos"
        putStrLn "Livros"
        putStrLn "Emprestimo"
        putStrLn "Sair"
        putStrLn "=========================================="
        opcao <- getLine
        return opcao