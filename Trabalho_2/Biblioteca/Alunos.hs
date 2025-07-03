module Biblioteca.Alunos where 
    import Biblioteca.Dados
    import System.IO
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
       
        cadastrar (Aluno cod nome email) = do -- Usamos '_' pois o argumento não é relevante aqui
            handle <- openFile "alunos.txt" AppendMode
            putStrLn "=========================================="
            putStrLn "           CADASTRO DE ALUNO"
            putStrLn "------------------------------------------"

            putStrLn "Digite o código do aluno:"
            codigoStr <- getLine
            let codigoAluno = read codigoStr :: Int -- Converte String para Int

            putStrLn "Digite o nome do aluno:"
            nomeAluno <- getLine

            putStrLn "Digite o email do aluno:"
            emailAluno <- getLine

            hPutStrLn handle (show codigoAluno ++ ", " ++ nomeAluno ++ ", " ++ emailAluno)
            hClose handle

            putStrLn "\nAluno cadastrado com sucesso!"
            putStrLn "=========================================="


    