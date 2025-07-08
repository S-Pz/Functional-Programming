module Biblioteca.Alunos (
    Aluno (..),
    imprimir,
    cadastrar,
    obter,
    buscar,
    apagar,
    showMenu
) where 
    
    import System.IO
    import Data.Proxy
    
    import Biblioteca.Dados
    import Biblioteca.Util 
    
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

            listaDeAlunos <- (obter :: IO (Set Aluno))
            
            if buscar codigoAluno listaDeAlunos /= Nothing
                then putStrLn "Codigo de aluno ja existente!"
                else do
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
            let linhas = dividirPorNovaLinha conteudo
            return (criaSetAlunos linhas)
        
        buscar cod (Set []) = Nothing
        buscar cod (Set(a:as)) 
            | codigo a == cod = Just a
            | otherwise = buscar cod (Set as)
        
        apagar _ cod = do
            empretimoConteudo <- readFile "emprestimos.txt"
            let emprestimos = dividirPorNovaLinha  empretimoConteudo

            let temPendencia = qualquer (alunoTemEmprestimo cod) emprestimos

            if temPendencia then
                putStrLn "Este aluno não pode ser apagado, pois possui um empréstimo"
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
            putStrLn "Visualizar"
            putStrLn "Apagar"
            putStrLn "Voltar"
            putStrLn "=========================================="
            putStrLn "Escolha uma opção: "
            opcao <- getLine
            
            case opcao of
                "Cadastrar" -> do
                    putStrLn "Você escolheu Cadastrar Aluno."
                    cadastrar (Aluno 0 "" "")
                    showMenu (Proxy :: Proxy Aluno)
                    return "Cadastrar"
                "Visualizar" -> do
                    putStrLn "Você escolheu Vizualizar Alunos."
                    setAlunos <- obter :: IO (Set Aluno)
                    imprimirSet setAlunos
                    showMenu (Proxy :: Proxy Aluno)
                    return "Visualizar"
                "Apagar" -> do
                    putStrLn "Você escolheu Apagar Aluno."
                    putStrLn "Digite o código do aluno a ser apagado:"
                    codStr <- getLine
                    let cod = read codStr :: Int
                    apagar dummyAluno cod
                    showMenu (Proxy :: Proxy Aluno)
                    return "Apagar"
                "Voltar" -> do
                    putStrLn "Voltando ao menu principal..."
                    return "Voltar"
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    showMenu (Proxy :: Proxy Aluno)
                    return "Invalido"
            return opcao
                
    criaSetAlunos :: [String] -> Set Aluno
    criaSetAlunos [] = Set []
    criaSetAlunos ("":ls) = criaSetAlunos ls
    criaSetAlunos (l:ls) = inserir aluno (criaSetAlunos ls)
        where
            partes = splitPor ',' l
            aluno = Aluno (read (partes !! 0)) (partes !! 1) (partes !! 2)
    
    alunoParaLinha :: Aluno -> String
    alunoParaLinha (Aluno cod nome email) = show cod ++ ", " ++ nome ++ ", " ++ email

    setParaString :: Set Aluno -> String
    setParaString (Set alunos) = juntarComNovaLinha (map alunoParaLinha alunos)

    -- alunoTemEmprestimo :: Int -> String -> Bool
    -- alunoTemEmprestimo codAluno linha = extrairCampo ',' 1 linha == show codAluno

    alunoTemEmprestimo :: Int -> String -> Bool
    alunoTemEmprestimo codAluno linha = (length partes > 1) && ((partes !! 1) == show codAluno)
        where
            partes = splitPor ',' linha