import Utils

main :: IO ()
main = do
    -- Test the problematic input
    let input = "\""
    
    putStrLn $ "Testing input: " ++ show input
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)
    putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input)
    
    -- Let's also test some edge cases
    let edgeCases = 
          [ ("\"", "Single quote")
          , ("'", "Single single quote")
          , ("\"\\", "Quote with backslash")
          , ("'\\", "Single quote with backslash")
          , ("\"\\\"", "Quote with escaped quote")
          , ("'\\'", "Single quote with escaped quote")
          ]
    
    putStrLn "\nTesting edge cases:"
    mapM_ (\(input, description) -> do
        putStrLn $ description ++ " " ++ show input ++ ":"
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input)
        ) edgeCases