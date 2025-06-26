module Main where
import Biblioteca.Util

main :: IO()
main = do
    putStrLn "Main..."
    let data1 = Data { dia = 5, mes = 3, ano = 2025 }
    putStrLn (dataStr data1)