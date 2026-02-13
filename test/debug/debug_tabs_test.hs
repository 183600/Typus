import Utils

main :: IO ()
main = do
    -- Test with " "
    let s1 = " "
    let withTabs1 = "\t\t" ++ s1 ++ "\t"
    let result1 = normalizeIndentation withTabs1
    putStrLn $ "Test 1 - s = \" \":"
    putStrLn $ "  With tabs: " ++ show withTabs1
    putStrLn $ "  Output: " ++ show result1
    putStrLn $ "  Expected: " ++ show withTabs1
    putStrLn $ ""
    
    -- Test with "\DEL"
    let s2 = "\DEL"
    let withTabs2 = "\t\t" ++ s2 ++ "\t"
    let result2 = normalizeIndentation withTabs2
    putStrLn $ "Test 2 - s = \"\\DEL\":"
    putStrLn $ "  With tabs: " ++ show withTabs2
    putStrLn $ "  Output: " ++ show result2
    putStrLn $ "  Expected: " ++ show withTabs2