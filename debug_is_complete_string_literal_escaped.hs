-- Test the failing case
import qualified Utils as U

main :: IO ()
main = do
    let s = "\""
    let escaped = "\"" ++ s ++ "\\\"\""
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Escaped: " ++ show escaped
    
    putStrLn $ "U.isCompleteStringLiteral escaped: " ++ show (U.isCompleteStringLiteral escaped)
    
    -- The test expects this to be True
    putStrLn $ "Test result: " ++ show (U.isCompleteStringLiteral escaped)