module Main where

import Data.Proxy

import Biblioteca.Alunos
import Biblioteca.Dados
import Biblioteca.Emprestimo
import Biblioteca.Livros
import Biblioteca.Util

main :: IO()
main = do
    showMainMenu

showMainMenu :: IO ()
showMainMenu = do
    putStrLn "=========================================="
    putStrLn "           MENU PRINCIPAL"
    putStrLn "------------------------------------------"
    putStrLn "SELECIONE O MENU DESEJADO:"
    putStrLn "Alunos"
    putStrLn "Livros"
    putStrLn "Emprestimo"
    putStrLn "Sair"
    putStrLn "=========================================="
    opcao <- getLine
    
    case opcao of
        "Alunos" -> do
            resultado <- showMenu (Proxy :: Proxy Aluno)
            putStrLn ""
        "Livros" -> do
            resultado <- showMenu (Proxy :: Proxy Livro)
            putStrLn ""
        "Emprestimo" -> do
            resultado <- showMenu (Proxy :: Proxy Emprestimo)
            putStrLn ""
        "Sair" -> do
            putStrLn "Encerrando sistema..."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            showMainMenu
    if opcao /= "Sair" 
        then showMainMenu
        else putStrLn "Obrigado por utilzar nosso sistema"