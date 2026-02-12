import Utils

main :: IO ()
main = do
    let s = "a\""
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed = " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed = " ++ show (isProblematicUnclosedString unclosed)
    putStrLn $ "Expected: closed=False, unclosed=True"