module Biblioteca.Livros where 
    import System.IO

    import Biblioteca.Dados
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
        
        cadastrar (Livro reg titulo edicao) = do
            
            putStrLn "=========================================="
            putStrLn "           CADASTRO DE LIVRO"
            putStrLn "------------------------------------------"

            putStrLn "Digite o registro do livro:"
            registroStr <- getLine
            
            let registroLivro = read registroStr :: Int

            putStrLn "Digite o título do livro:"
            tituloLivro <- getLine

            putStrLn "Digite a edição do livro:"
            edicaoStr <- getLine
            let edicaoLivro = read edicaoStr :: Int

            handle <- openFile "livros.txt" AppendMode
            hPutStrLn handle (show registroLivro ++ ", " ++ tituloLivro ++ ", " ++ show edicaoLivro)
            hClose handle

            putStrLn "\nLivro cadastrado com sucesso!"