import Utils

main :: IO ()
main = do
    let s = "\""
    let withSingle = "//" ++ s
    let processed = removeComments withSingle
    putStrLn $ "Input: " ++ show s
    putStrLn $ "WithSingle: " ++ show withSingle
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Expected: " ++ show s
    
    -- Test isProblematicUnclosedString
    putStrLn $ "isProblematicUnclosedString s: " ++ show (isProblematicUnclosedString s)
    putStrLn $ "isCompleteStringLiteral s: " ++ show (isCompleteStringLiteral s)