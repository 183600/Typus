import Utils

main :: IO ()
main = do
    let s = "a"
    let mixed = "  \t  " ++ s
    putStrLn $ "Input: " ++ show mixed
    let normalized = normalizeIndentation mixed
    putStrLn $ "Output: " ++ show normalized
    putStrLn $ "Contains tab: " ++ show ('\t' `elem` normalized)