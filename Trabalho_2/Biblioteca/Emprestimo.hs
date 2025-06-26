module Emprestimo where 
    data Emprestimo = Emprestimo {numero: Int, aluno: Aluno , data_emprestimo: Data, data_devolucao: Data, livros: [Livro]}
--            deriving (Eq, Show)