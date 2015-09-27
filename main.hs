module Main where

import JogoDaVelha (novoJogo)


main = iniciar

iniciar :: IO()
iniciar = do 
	putStrLn "Digite uma das opcoes abaixo e clique <Enter>: "
	putStrLn "1) Novo Jogo "
	putStrLn "2) Sair"
	opcao <- getChar
	getChar
	realizaOpcao opcao

realizaOpcao :: Char -> IO()
realizaOpcao '1' = do 
		novoJogo
		iniciar
realizaOpcao '2' = do
		putStrLn "Ateh logo!!"
		return ()