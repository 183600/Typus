import Utils (normalizeIndentation)

main :: IO ()
main = do
    let input = "\n\n"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (result == "    ")