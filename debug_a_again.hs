import qualified Utils as U

main :: IO ()
main = do
    -- Test the failing case
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    let withEscaped = "\"" ++ s ++ "\\\""
    
    putStrLn $ "Testing failing case:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    putStrLn $ "withEscaped = " ++ show withEscaped
    
    putStrLn $ "\nFunction results:"
    putStrLn $ "U.isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)
    
    let propertyResult = not (U.isProblematicUnclosedString closed) && 
                         U.isProblematicUnclosedString unclosed &&
                         U.isCompleteStringLiteral withEscaped
                         
    putStrLn $ "\nProperty result: " ++ show propertyResult