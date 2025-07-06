-- Arquivo: Main.hs (Novo)
module Main where

import Biblioteca.Alunos
import Biblioteca.Dados
import Biblioteca.Emprestimo
import Biblioteca.Livros
import Biblioteca.Util
import Data.Proxy

import Data.Maybe (isJust, fromJust)

-- Função auxiliar para imprimir o resultado da busca de forma elegante
imprimirBusca :: (Dado a) => Maybe a -> IO ()
imprimirBusca resultado =
    case resultado of
        Just item -> do
            putStrLn "\n>> Item encontrado:"
            imprimir item
        Nothing -> putStrLn "\n>> Item não encontrado."

main :: IO()
main = do
    showMainMenu
    --apagar (Aluno 0 "" "") 111

    -- #######  Testando as funções do Set ########
{-
    putStrLn "========================================"
    putStrLn "      INÍCIO: TESTANDO O MÓDULO SET"
    putStrLn "----------------------------------------"
    let s0 = vazio :: Set Int
    putStrLn $ "1. Criando um set vazio. `estaVazio`? -> " ++ show (estaVazio s0)

    let s1 = inserir 10 s0
    let s2 = inserir 20 s1
    putStrLn $ "2. Inserindo 10 e 20. Set atual: " ++ show s2

    let s3 = inserir 10 s2
    putStrLn $ "3. Tentando inserir 10 (duplicado). O Set não deve mudar: " ++ show s3

    putStrLn $ "4. Buscando o elemento 20. `buscarSet`? -> " ++ show (buscarSet 20 s3)
    putStrLn $ "5. Buscando o elemento 30. `buscarSet`? -> " ++ show (buscarSet 30 s3)

    let s4 = remover 10 s3
    putStrLn $ "6. Removendo o elemento 10. Set atual: " ++ show s4
    putStrLn $ "7. O set está vazio? `estaVazio`? -> " ++ show (estaVazio s4)
    putStrLn "----------------------------------------"
    putStrLn "        FIM: TESTANDO O MÓDULO SET"
    putStrLn "========================================\n"
-}


    -- ####### Testando cadastrar, obter e buscar para cada tipo de Dado ########
{-
    putStrLn "========================================"
    putStrLn "   TESTANDO CADASTRO, OBTENÇÃO E BUSCA"
    putStrLn "----------------------------------------"
    putStrLn "NOTA: Os cadastros salvam os dados em arquivos (alunos.txt, etc)."
    putStrLn "As funções de obter e buscar leem desses arquivos.\n"
-}
    -- --- Teste de Aluno ---
{-
    putStrLn "--- INÍCIO: TESTE DE ALUNO ---"
    putStrLn "\n(A) Testando 'cadastrar Aluno':"
    putStrLn "Por favor, insira os dados de um novo aluno quando solicitado."
    cadastrar (Aluno 0 "" "") -- Passa um valor dummy para acionar a função de cadastro interativo

    putStrLn "\n(B) Testando 'obter' e 'buscar' para Aluno:"
    putStrLn "Obtendo todos os alunos do arquivo 'alunos.txt'..."
    setAlunos <- obter :: IO (Set Aluno)
    putStrLn "Alunos obtidos com sucesso. Conteúdo do Set de Alunos:"
    print setAlunos

    putStrLn "\nAgora, vamos buscar por um aluno. Digite o CÓDIGO de um aluno para buscar:"
    codAlunoStr <- getLine
    let codAluno = read codAlunoStr :: Int
    putStrLn $ "Buscando aluno com código " ++ show codAluno ++ "..."
    let resultadoAluno = buscar codAluno setAlunos
    imprimirBusca resultadoAluno
    putStrLn "--- FIM: TESTE DE ALUNO ---\n"
-}

    -- --- Teste de Livro ---
{-
    putStrLn "--- INÍCIO: TESTE DE LIVRO ---"
    putStrLn "\n(A) Testando 'cadastrar Livro':"
    putStrLn "Por favor, insira os dados de um novo livro quando solicitado."
    cadastrar (Livro 0 "" 0) -- Passa um valor dummy

    putStrLn "\n(B) Testando 'obter' e 'buscar' para Livro:"
    putStrLn "Obtendo todos os livros do arquivo 'livros.txt'..."
    setLivros <- obter :: IO (Set Livro)
    putStrLn "Livros obtidos com sucesso. Conteúdo do Set de Livros:"
    print setLivros

    putStrLn "\nAgora, vamos buscar por um livro. Digite o REGISTRO de um livro para buscar:"
    regLivroStr <- getLine
    let regLivro = read regLivroStr :: Int
    putStrLn $ "Buscando livro com registro " ++ show regLivro ++ "..."
    let resultadoLivro = buscar regLivro setLivros
    imprimirBusca resultadoLivro
    putStrLn "--- FIM: TESTE DE LIVRO ---\n"
-}

    -- --- Teste de Emprestimo ---
{-    
    putStrLn "--- INÍCIO: TESTE DE EMPRÉSTIMO ---"
    putStrLn "\n(A) Testando 'cadastrar Emprestimo':"
    putStrLn "Por favor, insira os dados de um novo empréstimo."
    putStrLn "IMPORTANTE: Use um código de aluno e registros de livros que já foram cadastrados."
    cadastrar (Emprestimo 0 (Aluno 0 "" "") (Data 1 1 2000) (Data 1 1 2000) []) -- Dummy

    putStrLn "\n(B) Testando 'obter' e 'buscar' para Empréstimo:"
    putStrLn "Obtendo todos os empréstimos do arquivo 'emprestimos.txt'..."
    -- É preciso obter novamente os alunos e livros para o 'obter' de Emprestimo funcionar
    setAlunosAtualizado <- obter :: IO (Set Aluno)
    setLivrosAtualizado <- obter :: IO (Set Livro)
    setEmprestimos <- obter :: IO (Set Emprestimo)
    putStrLn "Empréstimos obtidos com sucesso. Conteúdo do Set de Empréstimos:"
    print setEmprestimos

    putStrLn "\nAgora, vamos buscar por um empréstimo. Digite o NÚMERO de um empréstimo para buscar:"
    numEmpStr <- getLine
    let numEmp = read numEmpStr :: Int
    putStrLn $ "Buscando empréstimo com número " ++ show numEmp ++ "..."
    let resultadoEmprestimo = buscar numEmp setEmprestimos
    imprimirBusca resultadoEmprestimo
    putStrLn "--- FIM: TESTE DE EMPRÉSTIMO ---\n"
-}

    -- ####### Testando o menu interativo para Aluno ########
{-    
    opcao <- showMenu (Proxy :: Proxy Aluno)
    putStrLn $ "Você digitou: " ++ opcao
-}

    -- ####### Testando o menu interativo para Emprestimo ########
{-
    opcao <- showMenu (Proxy :: Proxy Emprestimo)
    putStrLn $ "Você digitou: " ++ opcao
-}

    -- ####### Testando o menu interativo para Livro ########
{-
    opcao <- showMenu (Proxy :: Proxy Livro)
    putStrLn $ "Você digitou: " ++ opcao
-}

showMainMenu :: IO ()
showMainMenu = do
    putStrLn "=========================================="
    putStrLn "           MENU PRINCIPAL"
    putStrLn "------------------------------------------"
    putStrLn "SELECIONE O MENU DESEJADO:"
    putStrLn "ALUNOS"
    putStrLn "LIVROS"
    putStrLn "EMPRÉSTIMOS"
    putStrLn "FECHAR"
    putStrLn "=========================================="
    opcao <- getLine
    case opcao of
        "ALUNOS" -> do
            resultado <- showMenu (Proxy :: Proxy Aluno)
            putStrLn ""
        "LIVROS" -> do
            resultado <- showMenu (Proxy :: Proxy Livro)
            putStrLn ""
        "EMPRÉSTIMOS" -> do
            resultado <- showMenu (Proxy :: Proxy Emprestimo)
            putStrLn ""
        "FECHAR" -> do
            putStrLn "Encerrando sistema..."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            showMainMenu
    if opcao /= "FECHAR" 
        then showMainMenu
        else putStrLn "Obrigado por utilzar nosso sistema"