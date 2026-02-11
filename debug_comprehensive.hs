import qualified Utils as U

main :: IO ()
main = do
    -- Test various cases to understand the pattern
    let testCases = ["", "a", "a\"", "\"", "\\", "'", "'\\"]
    
    putStrLn "Testing various s values:"
    putStrLn "s\t\tclosed\t\tunclosed\t\tclosed_result\tunclosed_result\texpected_closed\texpected_unclosed"
    putStrLn "--------------------------------------------------------------------------------------------"
    
    mapM_ testCase testCases

testCase :: String -> IO ()
testCase s = do
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    let closedResult = U.isProblematicUnclosedString closed
    let unclosedResult = U.isProblematicUnclosedString unclosed
    let expectedClosed = False  -- closed should never be problematic
    let expectedUnclosed = True   -- unclosed should always be problematic
    
    putStrLn $ show s ++ "\t\t" ++ 
               show closed ++ "\t\t" ++ 
               show unclosed ++ "\t\t" ++ 
               show closedResult ++ "\t\t" ++ 
               show unclosedResult ++ "\t\t" ++
               show expectedClosed ++ "\t\t" ++
               show expectedUnclosed