module Biblioteca.Livros where 
    data Livro = Livro {
        registro:: Int,
        titulo:: String,
        edicao:: Int
    } deriving (Eq, Show)
    instance Dado Livro where
        imprimir (Livro reg titulo edicao) = do
            putStrLn ("Registro: " ++ show reg)
            putStrLn ("Título: " ++ titulo)
            putStrLn ("Edição: " ++ show edicao)
        toString = show
        size _ = 1