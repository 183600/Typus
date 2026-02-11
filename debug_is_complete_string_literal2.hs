import Utils

main :: IO ()
main = do
    let testCases = 
            [ ("\"b\"", "b followed by quote")
            , ("\"b\\\"\"", "b followed by escaped quote")
            , ("\"b\"\"\"", "b followed by quote and quote")
            ]
    
    mapM_ (\(s, desc) -> do
        putStrLn $ desc ++ ": " ++ show s
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral s)
        putStrLn ""
        ) testCases
