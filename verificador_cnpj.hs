-- ** AINDA NAO TOTALMENTE OTIMIZADO ** --

-- Importacao dos modulos.
import Data.Char (digitToInt)

-- Pega um numero inteiro e retorna uma lista de inteiros.
int_lista :: Integer -> [Int]
int_lista n = map digitToInt (show n)

-- Verifica se o tamanho do numero esta correto.
verif_tamanho :: [Int] -> Bool
verif_tamanho n = if length(n) == 14
																					then True
																					else False

-- Verificao do primeiro digito valido
verif_primeira :: [Int] -> Bool
verif_primeira n = if numero_1 == n !! 11
																						then True
																						else False
 where valor_1 = ((n !! 0) * 2 + (n !! 1) * 3 + (n !! 2) * 4 + (n !! 3) * 5 + (n !! 4) * 6 + (n !! 5) * 7 + (n !! 6) * 8 + (n !! 7) * 9 + (n !! 8) * 2 + (n !! 9) * 3 + (n !! 10) * 4 + (n !! 11) * 5) `rem` 11
       numero_1 = if valor_1 >= 10 then 0 else (11 - valor_1)

-- Verificao do segundo digito valido
verif_segunda :: [Int] -> Bool
verif_segunda n = if numero_2 == n !! 12
																					then True
																					else False
 where valor_1 = ((n !! 0) * 2 + (n !! 1) * 3 + (n !! 2) * 4 + (n !! 3) * 5 + (n !! 4) * 6 + (n !! 5) * 7 + (n !! 6) * 8 + (n !! 7) * 9 + (n !! 8) * 2 + (n !! 9) * 3 + (n !! 10) * 4 + (n !! 11) * 5) `rem` 11
       valor_2 = (valor_1 * 2 + (n !! 0) * 3 + (n !! 1) * 4 + (n !! 2) * 5 + (n !! 3) * 6 + (n !! 4) * 7 + (n !! 5) * 8 + (n !! 6) * 9 + (n !! 7) * 2 + (n !! 8) * 3 + (n !! 9) * 4 + (n !! 10) * 5 + (n !! 11) * 6) `rem` 11
       numero_2 = if valor_2 >= 10 then 0 else 11 - valor_2

-- Verifica se todos os numeros sao iguais.
verif_num_iguais :: [Int] -> Bool
verif_num_iguais n = if math == 0
																								then True
																								else False
 where math = n !! 0 - n !! 1 + n !! 2 - n !! 3 + n !! 4 - n !! 5 + n !! 6 - n !! 7 + n !! 8 - n !! 9 + n !! 10 - n !! 11 + n !! 12 - n !! 13
	
main :: IO()
main = do
	putStrLn "Digite seu cnpj: "
	input <- getLine
	let n = (read input :: Integer)
	if verif_tamanho(int_lista(n)) == True && verif_primeira(int_lista(n)) == True && verif_segunda(int_lista(n)) == True && verif_num_iguais(int_lista(n)) == False
				then print("Esse CNPJ e valido!")
				else print("Esse CNPJ nao e valido!")
