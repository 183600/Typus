import qualified Utils as U

main :: IO ()
main = do
    -- Test the specific failing case from SimpleQuickCheckTestSuite
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Testing SimpleQuickCheckTestSuite version:"
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
    
    -- Test the specific failing input directly
    let failingInput = "a\""
    putStrLn $ "\nDirect test of failing input: " ++ show failingInput
    putStrLn $ "U.isProblematicUnclosedString failingInput: " ++ show (U.isProblematicUnclosedString failingInput)
    
    -- Check if failingInput matches unclosed pattern
    putStrLn $ "\nPattern matching:"
    putStrLn $ "failingInput == unclosed: " ++ show (failingInput == unclosed)
    
    -- Test other edge cases
    putStrLn $ "\nOther edge cases:"
    putStrLn $ "U.isProblematicUnclosedString \"\\\"\": " ++ show (U.isProblematicUnclosedString "\"")
    putStrLn $ "U.isProblematicUnclosedString \"\\\\\": " ++ show (U.isProblematicUnclosedString "\\")
    putStrLn $ "U.isProblematicUnclosedString \"\": " ++ show (U.isProblematicUnclosedString "")