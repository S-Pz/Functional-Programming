module Biblioteca.Main where
    
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

        imprimir alunoTeste
        imprimir livroTeste1
        imprimir emprestimoTeste

    