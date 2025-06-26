module Biblioteca.Dados where

    class Dados d where
        toString :: d -> String
        size :: d -> Int