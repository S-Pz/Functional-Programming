module Main where
    
import Biblioteca.Alunos
import Biblioteca.Dados
import Biblioteca.Emprestimo
import Biblioteca.Livros
import Biblioteca.Util

main :: IO()
main = do
    
    let alunoTeste = Aluno {
        codigo = 101,
        nome = "Maria Souza",
        email = "maria.s@provedor.com"
    }

    let livroTeste1 = Livro {
        registro = 202401,
        titulo = "Haskell para Iniciantes",
        edicao = 2
    }

    let livroTeste2 = Livro {
        registro = 202402,
        titulo = "Algoritmos em Haskell",
        edicao = 1
    }

    let dataEmprestimo = Data {
        dia = 15,
        mes = 6,
        ano = 2025
    }

    let dataDevolucao = Data {
        dia = 30,
        mes = 6,
        ano = 2025
    }

    let emprestimoTeste = Emprestimo {
            numero = 1, 
            aluno = alunoTeste, 
            data_emprestimo = dataEmprestimo, 
            data_devolucao = dataDevolucao, 
            livros = [livroTeste1, livroTeste2]
        }

    -- imprimir alunoTeste
    -- putStrLn ""
    -- imprimir livroTeste1
    -- putStrLn ""
    -- imprimir emprestimoTeste
    -- putStrLn ""
    -- putStrLn ""
    -- cadastrar (Aluno 102 "Luiz Souza" "luiz.souza@provedor.com")


    -- #######  Testando o Set Dado ########
    let s0 = vazio :: Set Int
    print $ estaVazio s0

    let s1 = inserir 10 s0
    let s2 = inserir 20 s1
    let s3 = inserir 10 s2 -- duplicado, não será inserido
    print s3

    print $ buscar 20 s3
    print $ buscar 30 s3

    let s4 = remover 10 s3
    print s4
    print $ estaVazio s4

    -- ############ FIM DO TESTE ##########