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

        showMenu _ = do
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
                    showMenu (Proxy :: Proxy Aluno)
                    return "Invalido"
            return opcao
        
        apagar _ codigoAlvo = do
            conteudo <- readFile "alunos.txt"
            c_emprestimos <- readFile "emprestimos.txt"
            let emprestimos = lines c_emprestimos
                existe_emprestimo = filter (\linha -> extrairCampo 1 linha == show codigoAlvo) emprestimos
            if existe_emprestimo == [] 
                then
                    do
                        let alunos = lines conteudo
                            linhasFiltradas = filter (\linha -> extrairCampo 0 linha /= show codigoAlvo) alunos
                        
                        writeFile "alunos.txt" (unlines linhasFiltradas)
                else putStrLn "Existem Pendencias"
                
                    

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