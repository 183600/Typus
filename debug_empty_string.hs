import Utils

main :: IO ()
main = do
    let input = ""
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Length of result: " ++ show (length result)