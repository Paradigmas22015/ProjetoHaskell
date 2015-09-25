import Data.List

type Nome = String
type Vez = Int
type Tabela = [Char]


cadastrarJogadores :: IO()
cadastrarJogadores = do
					putStrLn "Informe o nome do Jogador 1: ";
					jogador1 <- getLine;
					putStrLn "Informe o nome do Jogador 2: ";
					jogador2 <- getLine;
					iniciarJogo jogador1 jogador2

iniciarJogo :: String -> String -> IO()
iniciarJogo jogador1 jogador2 = do
					putStrLn ("\nPrepare-se para ver a batalha \""
						++ jogador1 ++ "é X" ++ " vs " ++ jogador2 ++ " é 0" ++ "\"... \n AEWWWW")
					rodarJogo ['1','2','3','4','5','6','7','8','9'] jogador1 jogador2 0

rodarJogo :: Tabela -> Nome -> Nome -> Vez-> IO()
rodarJogo tabela jogador1 jogador2 vez =  do
					--{
					printarTabela tabela
					--Tem que saber se o jogador 1 venceu
					--Tem que saber se o jogador 2 venceu
					--Tem que saber se deu empate
					if (vez == 0) then do
						putStrLn (jogador1 ++ "eh a sua vez. Escolha uma posicao disponivel!")
						opcao <- getChar
						getLine -- descarta Enter
						rodarJogo (colocaXOuYNaTabela 'X' opcao tabela) jogador1 jogador2 1
					else do
						putStrLn (jogador2 ++ "eh a sua vez. Escolha uma posicao disponivel!")
						opcao <- getChar
						getLine -- descarta Enter
						rodarJogo (colocaXOuYNaTabela 'O' opcao tabela) jogador1 jogador2 0

					--colocaXOuYNaTabela é chamada
					--função dar loop até função verificaTabela retornar true ou se tiver dado velha
					--}
					printarTabela tabela
					putStrLn "O jogo foi encerrado "

printarTabela :: Tabela -> IO()
printarTabela tabela = do
					putStrLn ((show(tabela!!0)) ++ " | " ++ (show(tabela!!1)) ++ " | " ++ (show(tabela!!2)))
					putStrLn ((show(tabela!!3)) ++ " | " ++ (show(tabela!!4)) ++ " | " ++ (show(tabela!!5)))
					putStrLn ((show(tabela!!6)) ++ " | " ++ (show(tabela!!7)) ++ " | " ++ (show(tabela!!8)))
					putStrLn ("\n")
verificaEmpate :: Tabela -> Bool
verificaEmpate tabela
		 |((length (intersect "123456789" tabela)) == 0) = True
		 |otherwise = False

verificaTabelaZ :: Tabela -> Bool
verificaTabelaZ tabela
		--linha
		| (((tabela !! 0) == 'O') && ((tabela !! 1) == 'O') && ((tabela !! 2) == 'O')) = True
		| (((tabela !! 3) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 5) == 'O')) = True
		| (((tabela !! 6) == 'O') && ((tabela !! 7) == 'O') && ((tabela !! 8) == 'O')) = True
		--coluna
		| (((tabela !! 0) == 'O') && ((tabela !! 3) == 'O') && ((tabela !! 6) == 'O')) = True
		| (((tabela !! 1) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 7) == 'O')) = True
		| (((tabela !! 2) == 'O') && ((tabela !! 5) == 'O') && ((tabela !! 8) == 'O')) = True
		--diagonal
		| (((tabela !! 0) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 8) == 'O')) = True
		| (((tabela !! 2) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 6) == 'O')) = True

verificaTabelaX :: Tabela -> Bool
verificaTabelaX tabela
		--linha
		| (((tabela !! 0) == 'X') && ((tabela !! 1) == 'X') && ((tabela !! 2) == 'X')) = True
		| (((tabela !! 3) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 5) == 'X')) = True
		| (((tabela !! 6) == 'X') && ((tabela !! 7) == 'X') && ((tabela !! 8) == 'X')) = True
		--coluna
		| (((tabela !! 0) == 'X') && ((tabela !! 3) == 'X') && ((tabela !! 6) == 'X')) = True
		| (((tabela !! 1) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 7) == 'X')) = True
		| (((tabela !! 2) == 'X') && ((tabela !! 5) == 'X') && ((tabela !! 8) == 'X')) = True
		--diagonal
		| (((tabela !! 0) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 8) == 'X')) = True
		| (((tabela !! 2) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 6) == 'X')) = True
		| otherwise = False


colocaXOuYNaTabela :: Char -> Char -> Tabela -> Tabela
colocaXOuYNaTabela identificador posicao (h:t)
		| (h == posicao) = (identificador:t)
		| otherwise = h:(colocaXOuYNaTabela identificador posicao t)
		--putStrLn ("Essa função deverar colocar o identificado na posicao da tabela")
