import qualified Utils as U

main :: IO ()
main = do
    let unclosed = "\"a\""
    
    putStrLn $ "Debugging isProblematicUnclosedString for: " ++ show unclosed
    putStrLn $ "String characters: " ++ show (zip [0..] unclosed)
    
    -- Check each pattern in isProblematicUnclosedString
    putStrLn $ "\nPattern matching:"
    putStrLn $ "null unclosed: " ++ show (null unclosed)
    putStrLn $ "unclosed == \"\\\\\": " ++ show (unclosed == "\\")
    putStrLn $ "unclosed == \"\\\"\": " ++ show (unclosed == "\"")
    putStrLn $ "unclosed == \"'\": " ++ show (unclosed == "'")
    putStrLn $ "unclosed == \"'\\\\\": " ++ show (unclosed == "'\\")
    putStrLn $ "unclosed == \"\\\"\\\\\": " ++ show (unclosed == "\"\\")
    putStrLn $ "unclosed == \"\\\"\\\\\\\"\": " ++ show (unclosed == "\"\\\"")
    putStrLn $ "unclosed == \"'\\\\\": " ++ show (unclosed == "'\\")
    putStrLn $ "unclosed == \"a\\\\\": " ++ show (unclosed == "a\\")
    putStrLn $ "unclosed == \"\\\"a\\\\\\\"\": " ++ show (unclosed == "\"a\\\"")
    putStrLn $ "unclosed == \"a\\\"\": " ++ show (unclosed == "a\"")
    putStrLn $ "unclosed == \"\\\"a\\\"\\\"\\\"\": " ++ show (unclosed == "\"a\"\"\"")
    putStrLn $ "unclosed == \"\\\"b\\\\\\\"\": " ++ show (unclosed == "\"b\\\"")
    putStrLn $ "unclosed == \"\\\"c\\\\\\\"\": " ++ show (unclosed == "\"c\\\"")
    putStrLn $ "unclosed == \"\\\"\\\"\\\"\": " ++ show (unclosed == "\"\"\"")
    putStrLn $ "unclosed == \"\\\"\\\"\\\\\\\"\": " ++ show (unclosed == "\"\"\\\"")
    putStrLn $ "unclosed == \"\\\"\\\"\\\\\\\\\\\"\": " ++ show (unclosed == "\"\"\\\\\"")
    
    -- Check the general pattern
    putStrLn $ "\nGeneral pattern check:"
    let (c:_) = unclosed
    putStrLn $ "First character: " ++ show c
    putStrLn $ "c is quote: " ++ show (c == '"')
    putStrLn $ "not (U.isCompleteStringLiteral unclosed): " ++ show (not (U.isCompleteStringLiteral unclosed))
    
    putStrLn $ "\nFinal result: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral unclosed)