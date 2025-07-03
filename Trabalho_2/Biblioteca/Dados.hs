module Biblioteca.Dados where

class Dado d where
  imprimir :: d -> IO ()
  cadastrar :: d -> IO ()
  --buscar :: Eq t => t -> d -> Maybe t


data Set t = EmptySet | St t (Set t) deriving (Show)

instance (Show t, Eq t) => Dado (Set t) where
  imprimir s = putStrLn $ impr s
  --buscar = search

impr :: Show t => Set t -> String
impr (EmptySet) = ""
impr (St x EmptySet) = show x
impr (St x ls) = show x ++ "," ++ impr ls

insert :: Eq a => a -> Int -> Set a -> Set a          -- Insere um item na lista
insert x 0 (EmptySet) = St x EmptySet
insert _ _ (EmptySet) = error "Posição inválida"
insert x i (St y ls)
  | search x (St y ls) == Nothing = St y (insert x (i-1) ls)
  | otherwise = error "Item já existe"
  
remove :: Int -> Set a -> Set a                    -- Remove um item na lista
remove _ (EmptySet) = error "Lista Vazia"
remove 0 (St _ ls) = ls
remove x (St y ls) = St y (remove (x-1) ls)

search :: Eq a => a -> Set a -> Maybe a
search _ EmptySet = Nothing
search x (St y ls)
  | x == y    = Just y
  | otherwise = search x ls

setEmpty :: Set a -> Bool                        -- Verifica se a fila está vazia
setEmpty EmptySet = True
setEmpty _ = False
