import qualified Utils as U

main :: IO ()
main = do
    let s = "\r"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    putStrLn $ "Input: " ++ show mixed
    let result = U.normalizeIndentation mixed
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Match: " ++ show (result == "    ")