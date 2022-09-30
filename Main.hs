{- ALUNO: Rafael Vitagliano Tannenbaum Nuñez -}

{- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma
lista dos divisores de um número dado.
-}

divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1 .. n -1], mod n x == 0]


{-2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a
ocorrência de um caractere específico, em uma string dada.-}

contaCaractere :: Char -> String -> Int
contaCaractere x y = sum [1 | c <- y, x == c]

{- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve
o dobro dos valores dos elementos não negativos da lista de inteiros dada.-}

dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [x * 2 | x <- lista, x > 0]

{-4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista
de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem
construídos por inteiros entre 1 e um número inteiro dado.-}

pitagoras :: Int -> [(Int,Int,Int)]
pitagoras n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]]

{-5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número.
Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva
uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se
que você já tem uma função que devolve uma lista dos divisores de um número dado.
-}

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos x = [y | i <- [1 .. x], let y = sum $ divisoresden i, y == i]

{-6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o
produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = sum [fst x * snd x | x <- zip lista1 lista2]

{-7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva
uma lista contendo os n primeiros números primos a partir do número 2-}

numeroPrimo :: Int -> Bool
numeroPrimo n = divisoresden n == [1,n]

primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [x | x <- [2..], numeroPrimo x]

{-8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva
uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um
determinado número dado. Observe que estes números podem ser bem grandes.-}

paresOrdenados :: Int -> [(Int,Int)]
paresOrdenados n = [(x^2,x^3) | x <- [1..n]]

main = do
  putStrLn $ "\nExercício 1: divisoresden; Entrada: 15; Resultado: " ++ show (divisoresden 15)

  putStrLn $ "\nExercício 2: contaCaractere; Entrada: 'o' Formula 1; Resultado: " ++ show (contaCaractere 'o' "Formula 1")

  putStrLn $ "\nExercício 3: dobroNaoNegativo; Entrada: [1, 2, -6, -9, -3, 8, 9, 10]; Resultado: " ++ show (dobroNaoNegativo [1, 2, -6, -9, -3, 8, 9, 10])

  putStrLn $ "\nExercício 4: pitagoras; Entrada: 3; Resultado: " ++ show (pitagoras 3)

  putStrLn $ "\nExercício 5: numerosPerfeitos; Entrada: 500; Resultado: " ++ show (numerosPerfeitos 500)

  putStrLn $ "\nExercício 6: produtoEscalar; Entrada: [2, 3, 9, 7] [8, 4, 5, 10]; Resultado: " ++ show (produtoEscalar [2, 3, 9, 7] [8, 4, 5, 10])

  {-putStrLn $ "\nExercício 7: primeirosPrimos; Entrada: 5; Resultado: " ++ show (primeirosPrimos 5)-}

{-Por algum motivo, não consigo executar o exercício 7-}

  putStrLn $ "\nPor algum motivo, não consigo executar o exercício 7."

  putStrLn $ "\nExercício 8: paresOrdenados; Entrada: 5; Resultado: " ++ show (paresOrdenados 5)
