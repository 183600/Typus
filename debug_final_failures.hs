import qualified Utils as U

main :: IO ()
main = do
    -- Test the failing case for prop_is_problematic_unclosed_escape_quote
    let s = "a"
    let withEscape = "\"" ++ s ++ "\\\""
    
    putStrLn $ "Testing prop_is_problematic_unclosed_escape_quote:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "withEscape = " ++ show withEscape
    putStrLn $ "U.isProblematicUnclosedString withEscape: " ++ show (U.isProblematicUnclosedString withEscape)
    
    -- Test the failing case for prop_is_problematic_unclosed_string
    let s2 = ""
    let closed2 = "\"" ++ s2 ++ "\""
    let unclosed2 = "\"" ++ s2
    let withEscaped2 = "\"" ++ s2 ++ "\\\""
    
    putStrLn $ "\nTesting prop_is_problematic_unclosed_string:"
    putStrLn $ "s2 = " ++ show s2
    putStrLn $ "closed2 = " ++ show closed2
    putStrLn $ "unclosed2 = " ++ show unclosed2
    putStrLn $ "withEscaped2 = " ++ show withEscaped2
    
    putStrLn $ "U.isProblematicUnclosedString closed2: " ++ show (U.isProblematicUnclosedString closed2)
    putStrLn $ "U.isProblematicUnclosedString unclosed2: " ++ show (U.isProblematicUnclosedString unclosed2)
    putStrLn $ "U.isCompleteStringLiteral withEscaped2: " ++ show (U.isCompleteStringLiteral withEscaped2)
    
    let propertyResult2 = not (U.isProblematicUnclosedString closed2) && 
                          U.isProblematicUnclosedString unclosed2 &&
                          U.isCompleteStringLiteral withEscaped2
    putStrLn $ "Property result: " ++ show propertyResult2