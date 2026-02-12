import Utils

main :: IO ()
main = do
    let input = "\n\n"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Equal to expected: " ++ show (result == "    ")