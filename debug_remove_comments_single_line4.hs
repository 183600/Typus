-- Test what the test is actually testing
import qualified Utils as U

main :: IO ()
main = do
    -- The test prepends "//" to the string and expects it to be removed
    let s = "\n\983220"
    let withSingle = "//" ++ s
    putStrLn $ "Original string: " ++ show s
    putStrLn $ "With // prefix: " ++ show withSingle
    putStrLn $ "Length of withSingle: " ++ show (length withSingle)
    
    -- The test expects this to return empty string
    let result = U.removeComments withSingle
    putStrLn $ "Result: " ++ show result
    
    -- Let's see what happens if we treat the entire thing as a comment
    putStrLn "\nIf we treat the entire thing as a comment, it should be removed"
    
    -- What about a simpler case?
    putStrLn "\nSimple case:"
    let simple = "//hello"
    putStrLn $ "Input: " ++ show simple
    putStrLn $ "Result: " ++ show (U.removeComments simple)
    
    -- What about with newline?
    putStrLn "\nWith newline:"
    let withNewline = "//hello\nworld"
    putStrLn $ "Input: " ++ show withNewline
    putStrLn $ "Result: " ++ show (U.removeComments withNewline)