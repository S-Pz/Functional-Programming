module Biblioteca.Emprestimo where
    import System.IO
    
    import Biblioteca.Alunos
    import Biblioteca.Livros
    import Biblioteca.Util
    import Biblioteca.Dados
    
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
            num<- getLine
            let numEmp = read num :: Int

            putStrLn "Digite o código do aluno:"
            codAlunoStr <- getLine

            putStrLn "Digite a data do empréstimo (ex: DD/MM/AAAA):"
            dataEmp <- getLine

            putStrLn "Digite a data de devolução (ex: DD/MM/AAAA):"
            dataDev <- getLine

            handle <- openFile "emprestimos.txt" AppendMode
            putStrLn "Adicionar Livros"
            listaRegistros <- adicionaEmprestimos
            putStrLn "Adicionar Livros"
        
        obter = do
            setAlunos <- Biblioteca.Alunos.obter
            setLivros <- Biblioteca.Livros.obter
        
            conteudo <- readFile "emprestimos.txt"
            let linhas = lines conteudo
            return (criaSetEmprestimos linhas setAlunos setLivros)
            
    criaSetEmprestimos :: [String] -> Set Aluno -> Set Livro -> Set Emprestimo
    criaSetEmprestimos [] _ _ = EmptySet
    criaSetEmprestimos (linha:resto) alunosSet livrosSet = St emp (criaSetEmprestimos resto alunosSet livrosSet)
        where
            partes = splitPor ',' linha
            num = read (head partes) :: Int
            codAluno = read (partes !! 1) :: Int
            dataEmp = parseData (partes !! 2)
            dataDev = parseData (partes !! 3)
            codLivros = splitPor ';' (partes !! 4)
            
            alunoEncontrado = fromJust (buscaAluno codAluno alunosSet)
            livrosEmprestados = buscaLivros codLivros livrosSet
            
            emp = Emprestimo num alunoEncontrado dataEmp dataDev livrosEmprestados
    
    buscaAluno :: Int -> Set Aluno -> Maybe Aluno
    buscaAluno _ EmptySet = Nothing
    buscaAluno cod (St a resto)
        | codigo a == cod = Just a
        | otherwise       = buscaAluno cod resto

    buscaLivros :: [String] -> Set Livro -> [Livro]
    buscaLivros [] _ = []
    buscaLivros (regStr:rs) set =
        case buscaLivro (read regStr) set of
            Just l  -> l : buscaLivros rs set
            Nothing -> buscaLivros rs set 

    buscaLivro :: Int -> Set Livro -> Maybe Livro
    buscaLivro _ EmptySet = Nothing
    buscaLivro reg (St l resto)
        | registro l == reg = Just l
        | otherwise         = buscaLivro reg resto

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