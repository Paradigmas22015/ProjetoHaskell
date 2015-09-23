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
						++ jogador1 ++ " vs " ++ jogador2 ++ "\"... \n AEWWWW")
					rodarJogo

rodarJogo :: IO()
rodarJogo = putStrLn "Rodando jogo"