import Utils

-- Test normalize indentation empty lines
test1 :: IO ()
test1 = do
    let input = ""
    let result = normalizeIndentation input
    putStrLn $ "Test 1 - normalize indentation empty lines:"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: not \"\""

-- Test is_problematic_unclosed_string with "\\"
test2 :: IO ()
test2 = do
    let input = "\\"
    let result = isProblematicUnclosedString input
    putStrLn $ "\nTest 2 - is_problematic_unclosed_string with \"\\\\\":"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: True"

-- Test is_complete_string_literal with "a\""
test3 :: IO ()
test3 = do
    let input = "a\""
    let result = isCompleteStringLiteral input
    putStrLn $ "\nTest 3 - is_complete_string_literal with \"a\\\"\":"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: False"

-- Test remove_line_comments_end with "'a"
test4 :: IO ()
test4 = do
    let input = "'a"
    let result = removeLineComments input
    putStrLn $ "\nTest 4 - remove_line_comments_end with \"'a\":"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"'a\" (not \"'a// comment\")"

-- Test normalize_indentation_tabs with "a\v"
test5 :: IO ()
test5 = do
    let input = "a\v"
    let result = normalizeIndentation input
    putStrLn $ "\nTest 5 - normalize_indentation_tabs with \"a\\v\":"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: something other than \"a\\v\""

-- Test normalize_indentation_multiline_mixed with ["\nB"]
test6 :: IO ()
test6 = do
    let input = "\nB"
    let result = normalizeIndentation input
    let resultLines = lines result
    putStrLn $ "\nTest 6 - normalize_indentation_multiline_mixed with [\"\\nB\"]"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Result lines: " ++ show resultLines
    putStrLn $ "Number of lines: " ++ show (length resultLines)
    putStrLn $ "Expected: 1 line"

main :: IO ()
main = do
    test1
    test2
    test3
    test4
    test5
    test6