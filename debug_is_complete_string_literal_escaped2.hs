-- Test what isCompleteStringLiteral is doing
import qualified Utils as U

main :: IO ()
main = do
    let s = "\""
    let escaped = "\"" ++ s ++ "\\\"\""
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Escaped: " ++ show escaped
    putStrLn $ "Length of escaped: " ++ show (length escaped)
    
    -- Check if it matches any of the special cases
    putStrLn $ "Does it match \"\\\"\\\"\"? " ++ show (escaped == "\"\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\"\"? " ++ show (escaped == "\"\\\"\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\\\\\"\"? " ++ show (escaped == "\"\\\\\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\\\\\\\\\"\"? " ++ show (escaped == "\"\\\\\\\"\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\\\\\\\\\\\\\"\"? " ++ show (escaped == "\"\\\\\\\\\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\"\"? " ++ show (escaped == "\"\\\"\"")
    putStrLn $ "Does it match \"\\\"\\\\\\\\\\\"\"? " ++ show (escaped == "\"\\\\\"")
    putStrLn $ "Does it match \"\\\"a\\\\\\\"\"? " ++ show (escaped == "\"a\\\"\"")
    
    -- Check what the function is doing
    putStrLn $ "U.isCompleteStringLiteral escaped: " ++ show (U.isCompleteStringLiteral escaped)