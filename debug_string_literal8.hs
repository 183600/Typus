import Utils

main :: IO ()
main = do
    -- Test the exact input from the test
    let input1 = "\"" ++ "\\" ++ "\""  -- "\"\\\""
        input2 = "\""                   -- "\""
    
    putStrLn $ "Testing input1 (from test): " ++ show input1
    putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input1)
    
    putStrLn $ "\nTesting input2 (from error): " ++ show input2
    putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input2)