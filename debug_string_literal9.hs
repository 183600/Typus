import Utils

main :: IO ()
main = do
    -- Test the failing input
    let input1 = "'"  -- Single quote
        input2 = "\""  -- Double quote
    
    putStrLn $ "Testing input1 (single quote): " ++ show input1
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input1)
    putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input1)
    
    putStrLn $ "\nTesting input2 (double quote): " ++ show input2
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input2)
    putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input2)