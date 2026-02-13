import Utils

main :: IO ()
main = do
    let input = "\f"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "normalizeIndentation input: " ++ show (normalizeIndentation input)
    putStrLn $ "Expected: ?"