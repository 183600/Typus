import qualified Utils as U

main :: IO ()
main = do
    let failingInput = "a\""
    putStrLn $ "Testing isProblematicUnclosedString with input: " ++ show failingInput
    putStrLn $ "Result: " ++ show (U.isProblematicUnclosedString failingInput)
    
    -- Also test the related functions
    let closed = "\"" ++ "a" ++ "\""
    let unclosed = "\"" ++ "a"
    let withEscaped = "\"" ++ "a" ++ "\\\""
    
    putStrLn $ "Closed: " ++ show closed ++ " -> " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "Unclosed: " ++ show unclosed ++ " -> " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "WithEscaped: " ++ show withEscaped ++ " -> " ++ show (U.isProblematicUnclosedString withEscaped)
    putStrLn $ "WithEscaped isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral withEscaped)
    
    -- Test the specific failing case
    putStrLn $ "\nDirect test for failing case:"
    putStrLn $ "isProblematicUnclosedString \"a\\\"\": " ++ show (U.isProblematicUnclosedString "a\"")
    
    -- Test what the test expects
    let s = "a"
    putStrLn $ "\nTest logic:"
    putStrLn $ "not (U.isProblematicUnclosedString closed): " ++ show (not (U.isProblematicUnclosedString closed))
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)