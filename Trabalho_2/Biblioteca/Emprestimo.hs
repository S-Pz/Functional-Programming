module Emprestimo where

    import Livros
    import Alunos
    import Util

    data Emprestimo = Emprestimo {numero:: Int, aluno:: Aluno , data_emprestimo:: Data, data_devolucao:: Data, livros:: [Livro]}
--            deriving (Eq, Show)