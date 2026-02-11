-- Debug script for specific pattern matching
import Utils as U

-- Test specific pattern matching
test1 :: IO ()
test1 = do
    let testCases = [
            ("\"b\\\"\"", "Double quote + b + escaped quote + quote"),
            ("\"a\\\"\"", "Double quote + a + escaped quote + quote"),
            ("\"c\\\"\"", "Double quote + c + escaped quote + quote")
            ]
    mapM_ (\(s, desc) -> do
        putStrLn $ desc ++ ": " ++ show s
        putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)
        putStrLn "") testCases

main :: IO ()
main = do
    putStrLn "=== Testing specific pattern matching ==="
    test1
