import qualified Utils as U

main :: IO ()
main = do
    -- Test case that's failing
    let s = "\n\983220"
    let withSingle = "//" ++ s
    putStrLn $ "Input: " ++ show withSingle
    
    -- Let's trace through skipLine behavior
    let result = U.removeComments withSingle
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"\" (empty string)"
    
    -- Test a simpler case
    let simple = "//x"
    putStrLn $ "\nSimple case: " ++ show simple
    putStrLn $ "Simple result: " ++ show (U.removeComments simple)
    
    -- Test with newline
    let withNewline = "//\nx"
    putStrLn $ "\nWith newline: " ++ show withNewline
    putStrLn $ "With newline result: " ++ show (U.removeComments withNewline)