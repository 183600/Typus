import qualified Utils as U

main :: IO ()
main = do
    -- Test the actual failing case
    let s = "a\""
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Testing actual failing case:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    
    putStrLn $ "\nFunction results:"
    putStrLn $ "U.isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- Test the property logic
    let propertyResult = if s == ""
                        then not (U.isProblematicUnclosedString closed) && 
                             U.isProblematicUnclosedString unclosed
                        else if s == "\""
                             then let properlyClosed = "\"\\\"\""  
                                      properlyUnclosed = "\""    
                                  in not (U.isProblematicUnclosedString properlyClosed) && 
                                     U.isProblematicUnclosedString properlyUnclosed
                        else if s == "\\"
                             then U.isProblematicUnclosedString closed &&  
                                  U.isProblematicUnclosedString unclosed
                             else not (U.isProblematicUnclosedString closed) && 
                                  U.isProblematicUnclosedString unclosed
                                  
    putStrLn $ "\nProperty result: " ++ show propertyResult
    
    -- The issue is that for s = "a\"", the property expects:
    -- not (U.isProblematicUnclosedString closed) && U.isProblematicUnclosedString unclosed
    -- But it's getting:
    -- U.isProblematicUnclosedString closed = True (should be False)
    -- U.isProblematicUnclosedString unclosed = True (correct)
    
    putStrLn $ "\nExpected: not (U.isProblematicUnclosedString closed) = " ++ show (not (U.isProblematicUnclosedString closed))
    putStrLn $ "Actual: U.isProblematicUnclosedString unclosed = " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "Combined: " ++ show (not (U.isProblematicUnclosedString closed) && U.isProblematicUnclosedString unclosed)