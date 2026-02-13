import Utils

main :: IO ()
main = do
    let input = "\f"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: \"\\f\""