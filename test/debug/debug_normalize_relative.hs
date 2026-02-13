import Utils

main :: IO ()
main = do
    let input = "b\t"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "normalizeIndentation input: " ++ show (normalizeIndentation input)
    putStrLn $ "Expected: \"b \""