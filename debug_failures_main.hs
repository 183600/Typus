-- Debug script for failing tests
import Utils as U

-- Test prop_is_complete_string_literal_escaped with "b"
test1 :: IO ()
test1 = do
    let s = "b"
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Escaped: " ++ show escaped
    putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral escaped)
    putStrLn ""

-- Test prop_normalize_indentation_multiline_mixed with ["",""]
test2 :: IO ()
test2 = do
    let lines' = ["",""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = U.normalizeIndentation (unlines withMixed)
    let normLines = lines normalized
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Norm lines: " ++ show normLines
    putStrLn $ "Length normLines: " ++ show (length normLines)
    putStrLn $ "Expected length: " ++ show (length lines')
    putStrLn ""

main :: IO ()
main = do
    putStrLn "=== Test 1: prop_is_complete_string_literal_escaped with \"b\" ==="
    test1
    
    putStrLn "=== Test 2: prop_normalize_indentation_multiline_mixed with [\"\"] ==="
    test2