module Biblioteca.Dados where

class Dados d where
  toString :: d -> String
  size :: d -> Int


data Set t = EmptySet | St t (Set t)

instance (Show t) => Show (Set t) where
  show ls = "[" ++ (impr ls) ++ "]" 

instance Eq a => Eq (Set a) where
  EmptySet == EmptySet = True
  St x xs == St y ys = x == y && xs == ys
  _ == _ = False



impr :: Show t => Set t -> String
impr (EmptySet) = ""
impr (St x EmptySet) = show x
impr (St x ls) = show x ++ "," ++ impr ls

insert :: a -> Int -> Set a -> Set a          -- Insere um item na lista
insert x 0 (EmptySet) = St x EmptySet
insert _ _ (EmptySet) = error "Posição inválida"
insert x 0 (St y ls) = St x (St y ls)
insert x i (St y ls) = St y (insert x (i-1) ls)

remove :: Int -> Set a -> Set a                    -- Remove um item na lista
remove _ (EmptySet) = error "Lista Vazia"
remove 0 (St _ ls) = ls
remove x (St y ls) = St y (remove (x-1) ls)

search :: Eq a => a -> Set a -> a                        -- Busca um item na lista
search _ (EmptySet) = error "Lista Vazia"
search x (St y ls)
  | x == y = y
  | otherwise = search x ls

setEmpty :: Set a -> Bool                        -- Verifica se a fila está vazia
setEmpty EmptySet = True
setEmpty _ = False