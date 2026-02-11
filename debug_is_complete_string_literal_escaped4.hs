-- Test if the special case is being matched
import qualified Utils as U

main :: IO ()
main = do
    let s = "\""
    let escaped = "\"" ++ s ++ "\\\"\""
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Escaped: " ++ show escaped
    
    -- Check if it matches the special case
    putStrLn $ "Does it match \"\\\"\\\"\\\"\"? " ++ show (escaped == "\"\"\\\"\"")
    
    -- Check what the function is doing
    putStrLn $ "U.isCompleteStringLiteral escaped: " ++ show (U.isCompleteStringLiteral escaped)