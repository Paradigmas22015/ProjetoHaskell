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
					rodarJogo ['1','2','3','4','5','6','7','8','9']

rodarJogo :: Tabela -> IO()
rodarJogo tabela =  do
							--{
							printarTabela tabela
							--Tem que saber se é a vez do jogador 1 ou jogador 2
							--Tem pedir o lugar onde inserir X ou 0
							--colocaXOuYNaTabela é chamada
							--função dar loop até função verificaTabela retornar true ou se tiver dado velha
							--}
							printarTabela tabela
							putStrLn "O jogo foi encerrado "

printarTabela :: Tabela -> IO()
printarTabela tabela = putStrLn ("Essa função deverar ter um loop que acada 3 loops da uma quebra de linha")

verificaTabela :: Tabela -> IO()
verificaTabela tabela = do
											putStrLn ("Essa função deverar verificar se alguem ganhou ou se deu velha")
											putStrLn ("Alem de printar quem ganhou ou se deu velha")
											--return True

--colocaXOuYNaTabela :: Char -> Char -> Tabela -> Tabela
--colocaXOuYNaTabela identificador posicao tabela =  	putStrLn ("Essa função deverar colocar o identificado na posicao da tabela")
