module Biblioteca.Alunos where 
    
    import System.IO
    
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
    
    criaSetAlunos :: [String] -> Set Aluno
    criaSetAlunos [] = SetVazio
    criaSetAlunos (l:ls) = St aluno (criaSetAlunos ls)
        where
            partes = splitPorVirgula l
            aluno = Aluno (read (head partes)) (partes !! 1) (partes !! 2)
    
    splitPorVirgula :: String -> [String]
    splitPorVirgula [] = [""]
    splitPorVirgula (c:cs)
        | c == ','  = "" : resto
        | otherwise = (c : head resto) : tail resto
        
        where
            resto = splitPorVirgula cs