import Utils

main :: IO ()
main = do
    -- Test the valid case
    let testCases = 
          [ ""
          , "a"
          , "hello"
          , "with\"escape"
          , "with\\escape"
          ]
    
    mapM_ (\s -> 
        let stringWithQuotes = "\"" ++ s ++ "\""
        in putStrLn $ show stringWithQuotes ++ ": " ++ show (isCompleteStringLiteral stringWithQuotes)
        ) testCases