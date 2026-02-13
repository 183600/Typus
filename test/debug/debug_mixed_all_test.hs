import Utils

main :: IO ()
main = do
    -- Test with "\t"
    let s1 = "\t"
    let mixed1 = "\t  \t  " ++ s1 ++ "  \t  "
    let result1 = normalizeIndentation mixed1
    putStrLn $ "Test 1 - s = \"\\t\":"
    putStrLn $ "  Mixed: " ++ show mixed1
    putStrLn $ "  Output: " ++ show result1
    putStrLn $ "  Expected: " ++ show mixed1
    putStrLn $ ""
    
    -- Test with "\f"
    let s2 = "\f"
    let mixed2 = "\t  \t  " ++ s2 ++ "  \t  "
    let result2 = normalizeIndentation mixed2
    putStrLn $ "Test 2 - s = \"\\f\":"
    putStrLn $ "  Mixed: " ++ show mixed2
    putStrLn $ "  Output: " ++ show result2
    putStrLn $ "  Expected: " ++ show mixed2
    putStrLn $ ""
    
    -- Test with "\r"
    let s3 = "\r"
    let mixed3 = "\t  \t  " ++ s3 ++ "  \t  "
    let result3 = normalizeIndentation mixed3
    putStrLn $ "Test 3 - s = \"\\r\":"
    putStrLn $ "  Mixed: " ++ show mixed3
    putStrLn $ "  Output: " ++ show result3
    putStrLn $ "  Expected: \"    \""