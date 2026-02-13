import Utils

main :: IO ()
main = do
    let input = "a\""
    let result = isCompleteStringLiteral input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: False"