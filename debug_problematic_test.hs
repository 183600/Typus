import Utils

main :: IO ()
main = do
    let input = "a\""
    let result = isProblematicUnclosedString input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: True"