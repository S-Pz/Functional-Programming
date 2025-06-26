module Biblioteca.Dados where

class Dados d where
    toString :: d -> String
    size :: d -> Int

insert :: a -> Int -> Set a -> Set a          -- Insere um item na lista
--remove :: a -> Set a -> Set a                    -- Remove um item na lista
--search :: a -> Set a -> a                        -- Busca um item na lista
--setEmpty :: Set a -> Bool                        -- Verifica se a fila está vazia

data Set t = EmptySet | St t (Set t)

instance (Show t) => Show (Set t) where
  show ls = "[" ++ (impr ls) ++ "]" 

impr :: Show t => Set t -> String
impr (EmptySet) = ""
impr (St x EmptySet) = show x
impr (St x ls) = show x ++ "," ++ impr ls

insert x 0 (EmptySet) = St x EmptySet
insert _ _ (EmptySet) = error "Posição inválida"
insert x 0 (St y ls) = St x (St y ls)
insert x i (St y ls) = St y (insert x (i-1) ls)