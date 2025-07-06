module Biblioteca.Alunos where 
    
    import System.IO
    import Data.Proxy
    import Biblioteca.Dados 
    
    data Aluno = Aluno {
        codigo:: Int, 
        nome:: String ,
        email:: String
    } deriving (Eq, Show)

    instance Dado Aluno where
        imprimir (Aluno cod nome email) = do
            putStrLn ("Código do Aluno: " ++ show cod)
            putStrLn ("Nome: " ++ nome)
            putStrLn ("Email: " ++ email)
       
        cadastrar (Aluno cod nome email) = do 
            putStrLn "=========================================="
            putStrLn "           CADASTRO DE ALUNO"
            putStrLn "------------------------------------------"

            putStrLn "Digite o código do aluno:"
            codigoStr <- getLine
            let codigoAluno = read codigoStr :: Int 

            putStrLn "Digite o nome do aluno:"
            nomeAluno <- getLine

            putStrLn "Digite o email do aluno:"
            emailAluno <- getLine

            handle <- openFile "alunos.txt" AppendMode
            hPutStrLn handle (show codigoAluno ++ ", " ++ nomeAluno ++ ", " ++ emailAluno)
            hClose handle

            putStrLn "\nAluno cadastrado com sucesso!"
            putStrLn "=========================================="
        
        obter = do
            conteudo <- readFile "alunos.txt"
            let linhas = lines conteudo
            return (criaSetAlunos linhas)
        
        buscar cod (Set []) = Nothing
        buscar cod (Set(a:as)) 
            | codigo a == cod = Just a
            | otherwise = buscar cod (Set as)
        
        apagar _ cod = do
            empretimoConteudo <- readFile "emprestimos.txt"
            let emprestimos = lines empretimoConteudo

            let temPendencia = any (\linha -> extrairCampo 1 linha == show cod) emprestimos

            if temPendencia then
                putStrLn "\n[!] - Este aluno não pode ser apagado, pois possui um empréstimo"
            else do
                setAlunos <- obter :: IO (Set Aluno)
                let aluno = buscar cod setAlunos
                case aluno of
                    Nothing -> putStrLn "Aluno não encontrado."
                    Just a -> do
                        let novoSet = remover a setAlunos
                        let conteudoParaSalvar = setParaString novoSet
                        writeFile "alunos.txt" conteudoParaSalvar
                        putStrLn "Aluno apagado com sucesso!"


        showMenu _ = do
            let dummyAluno = Aluno 0 "" ""

            putStrLn "=========================================="
            putStrLn "           MENU DE ALUNOS"
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
                    putStrLn "Você escolheu Cadastrar Aluno."
                    cadastrar (Aluno 0 "" "")
                    showMenu (Proxy :: Proxy Aluno)
                    return "Cadastrar"
                "vizualizar" -> do
                    putStrLn "Você escolheu Vizualizar Alunos."
                    setAlunos <- obter :: IO (Set Aluno)
                    print setAlunos
                    showMenu (Proxy :: Proxy Aluno)
                    return "Vizualizar"
                "apagar" -> do
                    putStrLn "Você escolheu Apagar Aluno."
                    putStrLn "Digite o código do aluno a ser apagado:"
                    codStr <- getLine
                    let cod = read codStr :: Int
                    apagar dummyAluno cod
                    showMenu (Proxy :: Proxy Aluno)
                    return "Apagar"
                "voltar" -> do
                    putStrLn "Voltando ao menu principal..."
                    return "Voltar"
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    showMenu (Proxy :: Proxy Aluno)
                    return "Invalido"
            return opcao
                    
    extrairCampo :: Int -> String -> String
    extrairCampo n linha =
        let partes = splitPorVirgula linha
        in (elementoNaPosicao n partes)

    elementoNaPosicao :: Int -> [String] -> String
    elementoNaPosicao 0 (x:_)  = x
    elementoNaPosicao n (_:xs) = elementoNaPosicao (n - 1) xs
    elementoNaPosicao _ []     = error "Índice fora da faixa"
    
    criaSetAlunos :: [String] -> Set Aluno
    criaSetAlunos [] = Set []
    criaSetAlunos (l:ls) = inserir aluno (criaSetAlunos ls)
        where
            partes = splitPorVirgula l
            aluno = Aluno (read (head partes)) (partes !! 1) (partes !! 2)
    
    splitPorVirgula :: String -> [String]
    splitPorVirgula [] = [""]
    splitPorVirgula (',' : xs) = "" : splitPorVirgula xs
    splitPorVirgula (x : xs) =
        let (y : ys) = splitPorVirgula xs
        in (x : y) : ys

    alunoParaLinha :: Aluno -> String
    alunoParaLinha (Aluno cod nome email) = show cod ++ ", " ++ nome ++ ", " ++ email

    setParaString :: Set Aluno -> String
    setParaString (Set alunos) = unlines (map alunoParaLinha alunos)