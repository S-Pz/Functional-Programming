module Biblioteca.Util where

data Data = Data {dia:: Int, mes:: Int , ano:: Int}
        deriving (Eq, Show)

dataStr:: Data -> String
dataStr (Data d m a) = formata d ++ "/" ++ formata m ++ "/" ++ show a
    where
        formata n = if n < 10 
            then 
                '0' : show n
            else
                show n

