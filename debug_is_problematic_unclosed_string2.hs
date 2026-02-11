-- Test what isProblematicUnclosedString is doing
import qualified Utils as U

main :: IO ()
main = do
    let s = "a\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Closed: " ++ show closed
    putStrLn $ "Unclosed: " ++ show unclosed
    
    -- Check what the function is doing
    putStrLn $ "Length of closed: " ++ show (length closed)
    putStrLn $ "closed !! 0: " ++ show (closed !! 0)
    putStrLn $ "closed !! (length closed - 1): " ++ show (closed !! (length closed - 1))
    putStrLn $ "closed !! (length closed - 2): " ++ show (closed !! (length closed - 2))
    
    -- Check the condition in the function
    let condition = length closed >= 4 && closed !! 0 == '"' && closed !! (length closed - 1) == '"' && closed !! (length closed - 2) == '\\'
    putStrLn $ "Condition for closed: " ++ show condition
    
    -- Check if it's a complete string literal
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (U.isCompleteStringLiteral closed)
    
    -- The function returns True if condition is True or if it starts with quote and is not a complete string literal
    let result = condition || (head closed `elem` ['"', '\''] && not (U.isCompleteStringLiteral closed))
    putStrLn $ "Function result: " ++ show result