-- Test for isCompleteStringLiteral
import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let testCases = [
            ("\"a\\\"", "Test case: \"a\\\\\""),
            ("\"\\\\\"", "Test case: \"\\\\\\\\\""),
            ("\"\\\\\\\"", "Test case: \"\\\\\\\\\\\\\""),
            ("\"", "Test case: \""),
            ("\"\"", "Test case: \"\\\""),
            ("a\\", "Test case: a\\\\")
            ]
    
    putStrLn "Testing isCompleteStringLiteral:"
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ ": " ++ show (isCompleteStringLiteral input)
        ) testCases