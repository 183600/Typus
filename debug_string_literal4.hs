import Utils

main :: IO ()
main = do
    -- Test the exact input from the failure message
    let testCase = "\"" ++ "\\"  -- This should be "\""
    
    putStrLn $ "Testing testCase: " ++ show testCase
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testCase)
    
    -- Let's trace through the function step by step
    let result = isCompleteStringLiteral testCase
    putStrLn $ "Expected: False"
    putStrLn $ "Actual: " ++ show result
    putStrLn $ "Test passes: " ++ show (result == False)
    
    -- Let's also test some edge cases
    let edgeCases = 
          [ ["\"", "\\"]      -- "\""
          , ["'", "\\"]       -- "'\\"
          , ["\""]            -- "\"" (just a quote)
          , ["'"]             -- "'" (just a single quote)
          ]
    
    putStrLn "\nTesting edge cases:"
    mapM_ (\caseChars -> 
        let caseStr = concat caseChars
        in putStrLn $ caseStr ++ ": " ++ show (isCompleteStringLiteral caseStr)
        ) edgeCases