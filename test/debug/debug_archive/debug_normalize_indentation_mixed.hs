import Utils

main :: IO ()
main = do
    let s = ""
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    putStrLn $ "Input: " ++ show mixed
    putStrLn $ "Output: " ++ show normalized
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test passes: " ++ show (normalized == "    ")