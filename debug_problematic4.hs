import Utils

main :: IO ()
main = do
    let s = "a\""
    let unclosed = "\"" ++ s
    putStrLn $ "s = " ++ show s
    putStrLn $ "unclosed = " ++ show unclosed
    putStrLn $ "length unclosed = " ++ show (length unclosed)
    putStrLn $ "unclosed !! 0 = " ++ show (unclosed !! 0)
    putStrLn $ "unclosed !! 1 = " ++ show (unclosed !! 1)
    putStrLn $ "unclosed !! 2 = " ++ show (unclosed !! 2)
    putStrLn $ "unclosed !! 3 = " ++ show (unclosed !! 3)
    
    let rest = tail unclosed
    putStrLn $ "rest = " ++ show rest
    putStrLn $ "length rest = " ++ show (length rest)
    putStrLn $ "last rest = " ++ show (last rest)
    putStrLn $ "rest !! (length rest - 2) = " ++ show (rest !! (length rest - 2))
    
    let result = isProblematicUnclosedString unclosed
    putStrLn $ "isProblematicUnclosedString unclosed = " ++ show result