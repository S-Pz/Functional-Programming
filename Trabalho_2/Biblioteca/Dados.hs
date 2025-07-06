module Biblioteca.Dados where

  import System.IO
  import Data.Maybe (isNothing)
  import Data.Proxy
  
  class Dado d where
    imprimir :: d -> IO ()
    cadastrar :: d -> IO ()
    obter :: IO (Set d)
    buscar :: Int -> Set d -> Maybe d
    apagar :: d-> Int -> IO()
    showMenu :: Proxy d -> IO String

  data Set t = Set [t] deriving (Show, Eq)


  --instance (Show t, Eq t) => Dado (Set t) where
    --imprimir s = putStrLn $ impr s
    --buscar = search

  -- impr :: Show t => Set t -> String
  -- impr (EmptySet) = ""
  -- impr (St x EmptySet) = show x
  -- impr (St x ls) = show x ++ "," ++ impr ls

  vazio :: Set a
  vazio = Set []

  remover :: Eq a => a -> Set a -> Set a
  remover x (Set xs) = Set (filter (/= x) xs)

  inserir :: Eq a => a -> Set a -> Set a
  inserir x (Set xs)
    | buscarSet x (Set xs) = Set xs  -- já está presente, não insere
    | otherwise   = Set (x : xs)

  buscarSet :: Eq a => a -> Set a -> Bool
  buscarSet _ (Set []) = False
  buscarSet x (Set (h:xs))
    | x == h = True
    | otherwise = buscarSet x (Set xs)

  estaVazio :: Set a -> Bool
  estaVazio (Set[]) = True
  estaVazio _ = False
