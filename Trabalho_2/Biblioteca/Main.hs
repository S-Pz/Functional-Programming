module Main where
import Biblioteca.Util
import Biblioteca.Dados

main :: IO()
main = do
    putStrLn "Main..."
    
    let lista1 :: Set Int
        lista1 = EmptySet
    let lista2 = setEmpty lista1
    --let lista3 = insert 100 1 lista2
    putStrLn ("Lista: " ++ show lista2)