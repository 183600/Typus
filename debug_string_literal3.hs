import Utils

main :: IO ()
main = do
    -- Test the exact input from the failure message
    let testCase = "\"" ++ "\\"  -- This should be "\"\""
        testCase2 = "\"" ++ "\\" ++ "\""  -- This should be "\"\\\""
    
    putStrLn $ "Testing testCase: " ++ show testCase
    putStrLn $ "Length: " ++ show (length testCase)
    putStrLn $ "Characters: " ++ show (zip [0..] testCase)
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testCase)
    
    putStrLn $ "\nTesting testCase2: " ++ show testCase2
    putStrLn $ "Length: " ++ show (length testCase2)
    putStrLn $ "Characters: " ++ show (zip [0..] testCase2)
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testCase2)
    
    -- Test what the test expects
    putStrLn $ "\nTest expects isCompleteStringLiteral \"\\\\\" to be False"
    putStrLn $ "Actual result: " ++ show (isCompleteStringLiteral testCase)