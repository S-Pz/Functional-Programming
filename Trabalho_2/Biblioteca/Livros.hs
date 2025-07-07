module Biblioteca.Livros where 
    
    import System.IO
    import Data.Proxy
    
    import Biblioteca.Dados
    import Biblioteca.Util
    
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
            let linhas = dividirPorNovaLinha conteudo
            return (criaSetLivros linhas)
        
        buscar reg (Set []) = Nothing
        buscar reg (Set(a:as)) 
            | registro a == reg = Just a
            | otherwise = buscar reg (Set as)
        
        apagar _ reg = do
            emprestimoConteudo <- readFile "emprestimos.txt"
            
            let emprestimos  = dividirPorNovaLinha emprestimoConteudo
            let temPendencia = qualquer (livroEstaEmprestado reg) emprestimos

            if temPendencia then
                putStrLn "Este livro não pode ser apagado, pois está emprestado."
            else do
                setLivros <- obter :: IO (Set Livro)
                let novoSet = buscar reg setLivros

                case novoSet of 
                    Nothing -> putStrLn "Livro com o registro informado não foi encontrado."
                    Just l -> do
                        let novoSet = remover l setLivros
                        let conteudoParaSalvar = setParaString novoSet
                        writeFile "livros.txt" conteudoParaSalvar
                        putStrLn "Livro apagado com sucesso!"
        showMenu _ = do
            let dummyLivro = Livro 0 "" 0

            putStrLn "=========================================="
            putStrLn "           MENU DE LIVROS"
            putStrLn "------------------------------------------"
            putStrLn "Cadastrar"
            putStrLn "Visualizar"
            putStrLn "Apagar"
            putStrLn "Voltar"
            putStrLn "=========================================="
            putStrLn "Escolha uma opção: "
            opcao <- getLine
            
            case opcao of
                "Cadastrar" -> do
                    putStrLn "Você escolheu Cadastrar Livro."
                    cadastrar (Livro 0 "" 0)
                    showMenu (Proxy :: Proxy Livro)
                    return "Cadastrar"
                "Visualizar" -> do
                    putStrLn "Você escolheu Vizualizar Livros."
                    setLivros <- obter :: IO (Set Livro)
                    print setLivros
                    showMenu (Proxy :: Proxy Livro)
                    return "Vizualizar"
                "Apagar" -> do
                    putStrLn "Você escolheu Apagar Livro."
                    putStrLn "Digite o registro do livro a ser apagado:"
                    regStr <- getLine
                    let reg = read regStr :: Int
                    apagar dummyLivro reg
                    showMenu (Proxy :: Proxy Livro)
                    return "Apagar"
                "Voltar" -> do
                    putStrLn "Voltando ao menu principal..."
                    return "Voltar"
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    showMenu (Proxy :: Proxy Livro)
                    return "Invalido"
            return opcao
    
    criaSetLivros :: [String] -> Set Livro
    criaSetLivros [] = Set []
    criaSetLivros ("":ls) = criaSetLivros ls
    criaSetLivros (l:ls) = inserir livro (criaSetLivros ls)
        where
            partes = splitPor ',' l
            livro = Livro (read (head partes)) (partes !! 1) (read (partes !! 2) :: Int)

    livroParaLinha :: Livro -> String
    livroParaLinha (Livro reg titulo edicao) = show reg ++ ", " ++ titulo ++ ", " ++ show edicao

    setParaString :: Set Livro -> String
    setParaString (Set livros) = juntarComNovaLinha (map livroParaLinha livros)

    livroEstaEmprestado :: Int -> String -> Bool
    livroEstaEmprestado regLivro linha = estaNaLista (show regLivro) listaDeCodigos
      where
        listaDeCodigos   = splitPor ';' codigosLivrosStr
        codigosLivrosStr = extrairCampo ',' 4 linha

        estaNaLista :: String -> [String] -> Bool
        estaNaLista _ [] = False 
        estaNaLista item (x:xs)
            | item == x = True 
            | otherwise = estaNaLista item xs 

