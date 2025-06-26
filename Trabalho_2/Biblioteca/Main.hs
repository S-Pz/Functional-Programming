module Main where
import Biblioteca.Util
import Biblioteca.Dados

main :: IO()
main = do
    putStrLn "Main..."
    
    let lista2 = insert 10 0 EmptySet
    putStrLn ("Lista após adição: " ++ show lista2)
    let lista3 = insert 100 1 lista2
    putStrLn ("Lista após segunda adição: " ++ show lista3)