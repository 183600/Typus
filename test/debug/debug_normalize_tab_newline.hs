import Utils

main :: IO ()
main = do
    let input = "\t  \n"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "normalizeIndentation input: " ++ show (normalizeIndentation input)
    putStrLn $ "Expected: \"\\n\""