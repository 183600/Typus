-- Test what the test is actually expecting
import qualified Utils as U

main :: IO ()
main = do
    let s = "\""
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Quoted: " ++ show quoted
    putStrLn $ "Incomplete: " ++ show incomplete
    
    putStrLn $ "U.isCompleteStringLiteral quoted: " ++ show (U.isCompleteStringLiteral quoted)
    putStrLn $ "U.isCompleteStringLiteral incomplete: " ++ show (U.isCompleteStringLiteral incomplete)
    
    -- The test expects this to be True
    let expected = U.isCompleteStringLiteral quoted && U.isCompleteStringLiteral incomplete
    putStrLn $ "Test result: " ++ show expected