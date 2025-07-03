module Biblioteca.Emprestimo where
    import Biblioteca.Alunos
    import Biblioteca.Livros
    import Biblioteca.Util
    import Biblioteca.Dados
    import System.IO
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
            putStrLn (formata "Data de Empréstimo" (show dataEmp))
            putStrLn (formata "Data de Devolução" (show dataDev))
            putStrLn "\n--- Aluno -----------------------------"
            imprimir aluno
            putStrLn "\n--- Livros Emprestados ---------------"
            mapM_ imprimir livros
            putStrLn "========================================="
        
        cadastrar (Emprestimo num aluno dataEmp dataDev livros) = do 
            handle <- openFile "emprestimos.txt" AppendMode
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

            putStrLn "Adicionar Livros"
            listaRegistros <- adicionaEmprestimos
            putStrLn "Adicionar Livros"
            