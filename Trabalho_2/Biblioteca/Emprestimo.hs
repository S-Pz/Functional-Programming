module Biblioteca.Emprestimo where
    import System.IO
    import Data.Maybe (fromJust)
    
    import Biblioteca.Alunos
    import Biblioteca.Livros
    import Biblioteca.Util
    import Biblioteca.Dados
    import Data.Proxy
    
    data Emprestimo = Emprestimo {
        numero:: Int,
        aluno:: Aluno,
        data_emprestimo:: Data,
        data_devolucao:: Data, 
        livros:: [Livro]
    } deriving (Eq, Show)

    adicionaEmprestimos :: IO [String]
    adicionaEmprestimos = do
        putStrLn "Digite o registro de um livro (ou deixe em branco para finalizar):"
        regStr <- getLine
        if regStr == "" then return []
        else do
            maisRegs <- adicionaEmprestimos
            return (regStr : maisRegs)
    
    instance Dado Emprestimo where
        imprimir (Emprestimo num aluno dataEmp dataDev livros) = do
            putStrLn "========================================="
            putStrLn "           DETALHES DO EMPRÉSTIMO"
            putStrLn "-----------------------------------------"
            putStrLn (formata "Número" (show num))
            putStrLn (formata "Data de Empréstimo" (dataStr dataEmp))
            putStrLn (formata "Data de Devolução" (dataStr dataDev))
            putStrLn "\n--- Aluno -----------------------------"
            imprimir aluno
            putStrLn "\n--- Livros Emprestados ---------------"
            mapM_ imprimir livros
            putStrLn "========================================="
        
        cadastrar (Emprestimo num aluno dataEmp dataDev livros) = do 
            putStrLn "=========================================="
            putStrLn "         CADASTRO DE EMPRÉSTIMO"
            putStrLn "------------------------------------------"

            putStrLn "Digite o número do empréstimo:"
            num <- getLine
            let numEmp = read num :: Int

            putStrLn "Digite o código do aluno:"
            codAlunoStr <- getLine

            putStrLn "Digite a data do empréstimo (ex: DD/MM/AAAA):"
            dataEmp <- getLine

            putStrLn "Digite a data de devolução (ex: DD/MM/AAAA):"
            dataDev <- getLine

            putStrLn "Adicionar Livros"
            listaRegistros <- adicionaEmprestimos
            
            let linhaEmprestimo = num ++ "," ++ codAlunoStr ++ "," ++ dataEmp ++ "," ++ dataDev ++ "," ++ joinPor ';' listaRegistros
        
            handle <- openFile "emprestimos.txt" AppendMode
            hPutStrLn handle linhaEmprestimo
            hClose handle

        obter = do

            setAlunos <- obter :: IO (Set Aluno)
            setLivros <- obter :: IO (Set Livro)
        
            conteudo <- readFile "emprestimos.txt"
            let linhas = lines conteudo
            return (criaSetEmprestimos linhas setAlunos setLivros)
        
        buscar num (Set []) = Nothing
        buscar num (Set (e:es))
            | numero e == num = Just e
            | otherwise = buscar num (Set es) 

        showMenu _ = do
            putStrLn "=========================================="
            putStrLn "           MENU DE EMPRESTIMOS"
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
                    putStrLn "Você escolheu Cadastrar Emprestimo."
                    cadastrar (Emprestimo 0 (Aluno 0 "" "") (Data 1 1 2000) (Data 1 1 2000) [])
                    showMenu (Proxy :: Proxy Emprestimo)
                    return "Cadastrar"
                "vizualizar" -> do
                    putStrLn "Você escolheu Vizualizar Emprestimos."
                    setAlunosAtualizado <- obter :: IO (Set Aluno)
                    setLivrosAtualizado <- obter :: IO (Set Livro)
                    setEmprestimos <- obter :: IO (Set Emprestimo)
                    print setEmprestimos
                    showMenu (Proxy :: Proxy Emprestimo)
                    return "Vizualizar"
                "apagar" -> do
                    putStrLn "Você escolheu Apagar Emprestimo."
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
                    showMenu (Proxy :: Proxy Aluno)
            -}      return "Apagar"
                "voltar" -> do
                    putStrLn "Voltando ao menu principal..."
                    return "Voltar"
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    showMenu (Proxy :: Proxy Emprestimo)
                    return "Invalido"
            return opcao
    
    criaSetEmprestimos :: [String] -> Set Aluno -> Set Livro -> Set Emprestimo
    criaSetEmprestimos [] _ _ = Set []
    criaSetEmprestimos (linha:resto) alunosSet livrosSet =
        case (alunoEncontrado, livrosEmprestados) of
            (Just alunoVal, livrosVal) ->
                inserir (Emprestimo num alunoVal dataEmp dataDev livrosVal) (criaSetEmprestimos resto alunosSet livrosSet)
            _ -> criaSetEmprestimos resto alunosSet livrosSet  
        where
            partes = splitPor ',' linha
            num = read (partes !! 0) :: Int
            codAluno = read (partes !! 1) :: Int
            dataEmp = parseData (partes !! 2)
            dataDev = parseData (partes !! 3)
            codLivros = splitPor ';' (partes !! 4)

            alunoEncontrado = buscar codAluno alunosSet
            livrosEmprestados = buscaLivros codLivros livrosSet

    buscaLivros :: [String] -> Set Livro -> [Livro]
    buscaLivros [] _ = []
    buscaLivros (regStr:rs) set =
        case buscar (read regStr) set of 
            Just l  -> l : buscaLivros rs set
            Nothing -> buscaLivros rs set 

    parseData :: String -> Data
    parseData str = Data (read d) (read m) (read a)
        where
            [d, m, a] = splitPor '/' str

    splitPor :: Char -> String -> [String]
    splitPor _ [] = [""]
    splitPor delimitador (c:cs)
        | c == delimitador = "" : resto
        | otherwise        = (c : head resto) : tail resto
            where
                resto = splitPor delimitador cs

    joinPor :: Char -> [String] -> String
    joinPor _ [] = ""
    joinPor _ [x] = x
    joinPor sep (x:xs) = x ++ [sep] ++ joinPor sep xs