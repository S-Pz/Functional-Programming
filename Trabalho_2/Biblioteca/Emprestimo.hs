module Biblioteca.Emprestimo where

    import Biblioteca.Livros
    import Biblioteca.Alunos
    import Biblioteca.Util

    data Emprestimo = Emprestimo {numero:: Int, aluno:: Aluno , data_emprestimo:: Data, data_devolucao:: Data, livros:: [Livro]}
--            deriving (Eq, Show)