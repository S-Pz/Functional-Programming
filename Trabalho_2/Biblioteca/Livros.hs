module Biblioteca.Livros where 
    
    import System.IO
    import Data.Proxy
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

        obter = do
            conteudo <- readFile "livros.txt"
            let linhas = lines conteudo
            return (criaSetLivros linhas)
        
        buscar reg (Set []) = Nothing
        buscar reg (Set(a:as)) 
            | registro a == reg = Just a
            | otherwise = buscar reg (Set as)

        showMenu _ = do
            putStrLn "=========================================="
            putStrLn "           MENU DE LIVROS"
            putStrLn "------------------------------------------"
            putStrLn "Cadastrar"
            putStrLn "Vizualizar"
            putStrLn "Apagar"
            putStrLn "Voltar"
            putStrLn "=========================================="
            putStrLn "Escolha uma opção: "
            opcao <- getLine
            case opcao of
                "cadastrar" -> do
                    putStrLn "Você escolheu Cadastrar Livro."
                    cadastrar (Livro 0 "" 0)
                    showMenu (Proxy :: Proxy Livro)
                    return "Cadastrar"
                "vizualizar" -> do
                    putStrLn "Você escolheu Vizualizar Livros."
                    setLivros <- obter :: IO (Set Livro)
                    print setLivros
                    showMenu (Proxy :: Proxy Livro)
                    return "Vizualizar"
                "apagar" -> do
                    putStrLn "Você escolheu Apagar Livro."
              {-      putStrLn "Digite o código do aluno a ser apagado:"
                    codStr <- getLine
                    let cod = read codStr :: Int
                    let setAlunos = obter :: IO (Set Aluno)
                    let aluno = buscar cod setAlunos
                    case aluno of
                        Just a -> do
                            let novoSet = remover a setAlunos
                            -- Aqui você deve salvar o novoSet no arquivo, se necessário.
                            putStrLn $ "Aluno com código " ++ show cod ++ " removido."
                        Nothing -> putStrLn $ "Aluno com código " ++ show cod ++ " não encontrado."
              -}
                    showMenu (Proxy :: Proxy Livro)
                    return "Apagar"
                "voltar" -> do
                    putStrLn "Voltando ao menu principal..."
                    return "Voltar"
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    showMenu (Proxy :: Proxy Livro)
                    return "Invalido"
            return opcao
    
    criaSetLivros :: [String] -> Set Livro
    criaSetLivros [] = Set []
    criaSetLivros (l:ls) = inserir livro (criaSetLivros ls)
        where
            partes = splitPorVirgula l
            livro = Livro (read (head partes)) (partes !! 1) (read (partes !! 2) :: Int)
    
    splitPorVirgula :: String -> [String]
    splitPorVirgula [] = [""]
    splitPorVirgula (c:cs)
        | c == ','  = "" : resto
        | otherwise = (c : head resto) : tail resto
        where resto = splitPorVirgula cs