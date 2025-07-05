module Biblioteca.Dados where

  import System.IO
  import Data.Maybe (isNothing)
  
  class Dado d where
    imprimir :: d -> IO ()
    cadastrar :: d -> IO ()
    obter :: IO (Set d)
    --buscar :: Eq t => t -> d -> Maybe t

data Set t = Set [t] deriving (Show, Eq)

instance Dado Int
instance Dado String

--instance (Show t, Eq t) => Dado (Set t) where
  --imprimir s = putStrLn $ impr s
  --buscar = search

-- impr :: Show t => Set t -> String
-- impr (EmptySet) = ""
-- impr (St x EmptySet) = show x
-- impr (St x ls) = show x ++ "," ++ impr ls

vazio :: Set a
vazio = Set []

remover :: Dado a => a -> Set a -> Set a
remover x (Set xs) = Set (filter (/= x) xs)

inserir :: Dado a => a -> Set a -> Set a
inserir x (Set xs)
  | buscar x (Set xs) = Set xs  -- já está presente, não insere
  | otherwise   = Set (x : xs)

buscar :: Dado a => a -> Set a -> Bool
buscar _ (Set []) = False
buscar x (Set (h:xs))
  | x == h = True
  | otherwise = buscar x (Set xs)


estaVazio :: Set a -> Bool
estaVazio (Set[]) = True
estaVazio _ = False
