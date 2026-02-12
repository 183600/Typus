import Utils

main :: IO ()
main = do
    let input = "\t  \t  \n  \t  "  -- "\n" case
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Equal: " ++ show (input == result)