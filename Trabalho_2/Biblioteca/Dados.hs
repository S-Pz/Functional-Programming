module Biblioteca.Dados where

  import System.IO
  import Data.Maybe (isNothing)
  
  class Dado d where
    imprimir :: d -> IO ()
    cadastrar :: d -> IO ()
    obter :: IO (Set d)
    --buscar :: Eq t => t -> d -> Maybe t

  data Set t = SetVazio | 
    St t (Set t) 
    deriving (Show)

  impr :: Show t => Set t -> String
  impr SetVazio = ""
  impr (St x SetVazio) = show x
  impr (St x ls) = show x ++ "," ++ impr ls

  insert :: Eq a => a -> Set a -> Set a        
  insert x s
    | isNothing (search x s)= St x s
    | otherwise = s 
    
  remove :: Eq a => a -> Set a -> Set a                
  remove _ SetVazio = error "Set Vazio"
  remove x (St y ls) 
    | x == y = ls
    | otherwise = St y (remove x ls)

  search :: Eq a => a -> Set a -> Maybe a
  search _ SetVazio = Nothing
  search x (St y ls)
    | x == y    = Just y
    | otherwise = search x ls

  setVazio :: Set a -> Bool                        
  setVazio SetVazio = True
  setVazio _ = False
