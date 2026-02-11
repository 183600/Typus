import qualified Utils as U

main :: IO ()
main = do
    -- Test the new failing case
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Testing new failing case:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    
    putStrLn $ "\nFunction results:"
    putStrLn $ "U.isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- Test the property logic from CoreUtilsQuickCheckTests.hs
    let withEscaped = "\"" ++ s ++ "\\\""
    putStrLn $ "withEscaped = " ++ show withEscaped
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)
    
    let propertyResult = not (U.isProblematicUnclosedString closed) && 
                         U.isProblematicUnclosedString unclosed &&
                         U.isCompleteStringLiteral withEscaped
                         
    putStrLn $ "\nProperty result: " ++ show propertyResult
    
    -- Check each component
    putStrLn $ "\nComponents:"
    putStrLn $ "not (U.isProblematicUnclosedString closed): " ++ show (not (U.isProblematicUnclosedString closed))
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)