import Utils

main :: IO ()
main = do
    let input = "a\""
    let result = isProblematicUnclosedString input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "isProblematicUnclosedString: " ++ show result