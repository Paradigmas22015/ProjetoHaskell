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


colocaXOuYNaTabela :: Char -> Char -> Tabela -> Tabela
colocaXOuYNaTabela identificador posicao (h:t)
		| (h == posicao) = (identificador:t)
		| otherwise = h:(colocaXOuYNaTabela identificador posicao t)
		--putStrLn ("Essa função deverar colocar o identificado na posicao da tabela")
