import Utils

main :: IO ()
main = do
    let input = "\"a\""
    let result = removeComments input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Quotes in input: " ++ show (length $ filter (== '"') input)
    putStrLn $ "Quotes in result: " ++ show (length $ filter (== '"') result)