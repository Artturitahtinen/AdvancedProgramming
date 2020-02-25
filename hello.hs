askNames :: IO (String, String)
askNames = do
    putStrLn "Enter player 1 name:"
    player1 <- getLine
    putStrLn "Enter player 2 name:"
    player2 <- getLine
    return (player1,player2)

main :: IO ()
main = do
    (player1,player2) <- askNames
    putStrLn ("Hello " ++ player1 ++ " and " ++ player2)
