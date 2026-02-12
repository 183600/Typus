import Utils

main :: IO ()
main = do
    let s = "\"a\""
    putStrLn $ "s = " ++ show s
    putStrLn $ "length s = " ++ show (length s)
    putStrLn $ "s !! 0 = " ++ show (s !! 0)
    putStrLn $ "s !! 1 = " ++ show (s !! 1)
    putStrLn $ "s !! 2 = " ++ show (s !! 2)
    putStrLn $ "last s = " ++ show (last s)
    
    let rest = tail s
    putStrLn $ "rest = " ++ show rest
    putStrLn $ "length rest = " ++ show (length rest)
    putStrLn $ "last rest = " ++ show (last rest)
    putStrLn $ "rest !! (length rest - 2) = " ++ show (rest !! (length rest - 2))
    
    let result = isProblematicUnclosedString s
    putStrLn $ "isProblematicUnclosedString s = " ++ show result