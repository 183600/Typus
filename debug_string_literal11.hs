import Utils

main :: IO ()
main = do
    -- Test the failing input
    let s = "\\"
        stringWithQuotes = "\"" ++ s ++ "\""  -- This should be "\"\\\""
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "stringWithQuotes: " ++ show stringWithQuotes
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral stringWithQuotes)
    
    -- Test what the test expects
    putStrLn $ "\nTest expects isCompleteStringLiteral \"\\\\\"\" to be True"
    putStrLn $ "Actual result: " ++ show (isCompleteStringLiteral stringWithQuotes)