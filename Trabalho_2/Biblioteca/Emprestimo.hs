module Biblioteca.Emprestimo where
    import Biblioteca.Alunos
    import Biblioteca.Livros
    import Biblioteca.Util

    data Emprestimo = Emprestimo {
        numero:: Int,
        aluno:: Aluno,
        data_emprestimo:: Data,
        data_devolucao:: Data, 
        livros:: [Livro]
    } deriving (Eq, Show)

    instance Dado Emprestimo where
        imprimir (Emprestimo num aluno dataEmp dataDev livros) = do
            putStrLn "========================================="
            putStrLn "           DETALHES DO EMPRÉSTIMO"
            putStrLn "-----------------------------------------"
            putStrLn (formata "Número" (show num))
            putStrLn (formata "Data de Empréstimo" (show dEmp))
            putStrLn (formata "Data de Devolução" (show dDev))
            putStrLn "\n--- Aluno -----------------------------"
            imprimir aluno
            putStrLn "\n--- Livros Emprestados ---------------"
            mapM_ imprimir livros
            putStrLn "========================================="
        toString = show
        size _ = 1