module Util where 
    data Data = Data {dia: Int, mes: Int , ano: Int}
            deriving (Eq, Show)

dataStr:: Data -> String
dataStr (Data d m a) = Formata d ++ "/" ++ Formata m ++ "/" ++ Show a
    where
        Formata n = if n < 10 
                        then 
                            '0' : Show n
                    else
                        Show n

