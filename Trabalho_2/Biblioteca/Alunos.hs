module Alunos where 
    import Biblioteca.Dados
    data Aluno = Aluno {
        codigo:: Int, 
        nome:: String ,
        email:: String
    } deriving (Eq, Show)

    instance Dado Aluno where
        imprimir (Aluno cod nome email) = do
            putStrLn ("Código do Aluno: " ++ show codigo)
            putStrLn ("Nome: " ++ nome)
            putStrLn ("Email: " ++ email)
        toString = show
        size _= 1