-- 1. Crie uma função polimórfica que recebe uma lista 
-- e retorna o menor e o maior valor dessa lista.

lista :: [a] -> [Char]
lista lst = show maior lst ++ show menor lst

maior :: [a] -> a
maior [x] = x
maior (a:b) = if a >= maiorCauda then a 
             else maiorCauda
        where 
            maiorCauda = maior b

menor :: [a] -> a
menor [x] = x
menor (a:b) = if a <= menorCauda then a
             else menorCauda
        where
            menorCauda = menor b

2. Crie uma função polimórfica de alta ordem que recebe uma função unária que retorna Bool e uma lista 
e retorna uma tupla, cujo primeiro elemento é uma lista dos
elementos que obtiveram True com a função recebida e o segundo elemento é 
uma lista dos elementos que obtiveram False. 
Ex: fun odd [1,2,3,4] = ([1,3],[2,4])


3. Crie o tipo Pacote que pode assumir os valores Vazio ou um conteúdo de umtipoqualquer.
Crie uma função que desempacota o conteúdo de um Pacote caso não esteja vazio(useotipo Maybe). 
Indique que o tipo Pacote é uma instância de Show, cuja string obtidadeveserdo conteúdo dentro de um quadrado.
Por exemplo, uma caixa como valor 100deveserimpressa como: -------

| 100 |
-------
4. Usando o conceito de tipos algébricos, crie o tipo Pessoa, que pode ser uma pessoafísicacom nome e cpf ou uma pessoa jurídica com nome e cnpj.
Indique que Pessoaéumainstância das classes Show e Eq. 
5. Crie a função isPF que retorna True se uma pessoa for física e False se for jurídica. 
6. Indique que pessoa é uma instância da classe Ord, ordenando as pessoas combasenaordem alfabética do nome.
Dica: o tipo String já é uma instância de Ord. 
7. Crie uma classe de tipos chamada Unico com as funções: 
igual, que indica se doisvaloressão iguais caso o componente identificador deles for igual; 
e checkID que recebe umalistadevalores e retorna essa lista com apenas a primeira ocorrência de umvalor comdeterminadoidentificador. 
8. Indique que Pessoa é uma instância da classe Unico.