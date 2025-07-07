module Main where

import Data.Proxy

import Biblioteca.Alunos
import Biblioteca.Dados
import Biblioteca.Emprestimo
import Biblioteca.Livros
import Biblioteca.Util

main :: IO()
main = do
    opcao <- showMainMenu
    
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
            opcao <- showMainMenu
            putStrLn ""
    if opcao /= "Sair" 
        then main
        else putStrLn "Obrigado por utilzar nosso sistema"
            
        