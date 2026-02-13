import qualified Utils as U

main :: IO ()
main = do
    putStrLn "=== Test 1: Mixed tabs and spaces with carriage return ==="
    let s = "\r"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    putStrLn $ "Input: " ++ show mixed
    let result1 = U.normalizeIndentation mixed
    putStrLn $ "Output: " ++ show result1
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Match: " ++ show (result1 == "    ")
    
    putStrLn "\n=== Test 2: Empty string ==="
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = U.normalizeIndentation (unlines withMixed)
    
    putStrLn $ "Testing lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test result: " ++ show (normalized == "    ")