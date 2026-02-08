import Utils

main :: IO ()
main = do
    -- Test various inputs to understand the issue
    let testCases = 
          [ ("\"", "Single quote")
          , ("\"", "Single quote (explicit)")
          , ("\"\\", "Quote with backslash")
          , ("\"\\", "Quote with backslash (explicit)")
          , ("\"a", "Quote with a")
          , ("\"a\"", "Complete string")
          ]
    
    mapM_ (\(input, description) -> 
        putStrLn $ description ++ " " ++ show input ++ ": " ++ show (isCompleteStringLiteral input)
        ) testCases