import qualified Utils as U
import Data.List (isPrefixOf)

-- 测试 prop_normalize_indentation_tabs 失败用例
test_normalize_indentation_tabs :: IO ()
test_normalize_indentation_tabs = do
  let s = "a"
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = U.normalizeIndentation withTabs
  putStrLn $ "Input: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Expected: not (\"\\t\\t\" `isPrefixOf` normalized)"
  putStrLn $ "Actual: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))
  
  -- 测试 normalizeIndentation 的行为
  putStrLn "\n=== Testing normalizeIndentation with tabs ==="
  let testInput = "\t\ta\t"
  let testOutput = U.normalizeIndentation testInput
  putStrLn $ "Test input: " ++ show testInput
  putStrLn $ "Test output: " ++ show testOutput
  putStrLn $ "Has tabs at start: " ++ show ("\t\t" `isPrefixOf` testOutput)

-- 测试 prop_remove_line_comments_multiline 失败用例
test_remove_line_comments_multiline :: IO ()
test_remove_line_comments_multiline = do
  let lines' = ["\n"]
  let code = unlines lines'
  let processed = U.removeLineComments code
  let procLines = lines processed
  putStrLn $ "Input lines': " ++ show lines'
  putStrLn $ "code: " ++ show code
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "procLines: " ++ show procLines
  putStrLn $ "Expected length: 1"
  putStrLn $ "Actual length: " ++ show (length procLines)
  putStrLn $ "Test passes: " ++ show (length procLines == 1)
  
  -- 测试 removeLineComments 的行为
  putStrLn "\n=== Testing removeLineComments with [\"\\n\"] ==="
  let testInput = "\n\n"
  let testOutput = U.removeLineComments testInput
  let testLines = lines testOutput
  putStrLn $ "Test input: " ++ show testInput
  putStrLn $ "Test output: " ++ show testOutput
  putStrLn $ "Test lines: " ++ show testLines
  putStrLn $ "Length of test lines: " ++ show (length testLines)

-- 测试 prop_is_complete_string_literal 失败用例
test_is_complete_string_literal :: IO ()
test_is_complete_string_literal = do
  let s = "a\""
  let quoted = "\"" ++ s ++ "\""
  let incomplete = "\"" ++ s
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "quoted: " ++ show quoted
  putStrLn $ "incomplete: " ++ show incomplete
  putStrLn $ "isCompleteStringLiteral quoted: " ++ show (U.isCompleteStringLiteral quoted)
  putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (U.isCompleteStringLiteral incomplete)
  putStrLn $ "Expected: quoted=True, incomplete=False"
  putStrLn $ "Test passes: " ++ show (U.isCompleteStringLiteral quoted && not (U.isCompleteStringLiteral incomplete))
  
  -- 测试 isCompleteStringLiteral 的行为
  putStrLn "\n=== Testing isCompleteStringLiteral with \"\\\"a\\\"\\\"\" ==="
  let testInput1 = "\"a\"\""
  let testOutput1 = U.isCompleteStringLiteral testInput1
  putStrLn $ "Test input1: " ++ show testInput1
  putStrLn $ "Test output1: " ++ show testOutput1
  
  putStrLn "\n=== Testing isCompleteStringLiteral with \"\\\"a\"\" ==="
  let testInput2 = "\"a\""
  let testOutput2 = U.isCompleteStringLiteral testInput2
  putStrLn $ "Test input2: " ++ show testInput2
  putStrLn $ "Test output2: " ++ show testOutput2

-- 测试 normalizeIndentation code block 失败用例
test_normalize_indentation_code_block :: IO ()
test_normalize_indentation_code_block = do
  let s = ""
  let codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
  let normalized = U.normalizeIndentation codeBlock
  let normLines = lines normalized
  let nonCommentLines = filter (not . isPrefixOf "//") normLines
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "nonCommentLines: " ++ show nonCommentLines
  putStrLn $ "Expected: length (filter (isPrefixOf \"    \") normLines) < length normLines"
  putStrLn $ "Actual: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines)
  putStrLn $ "Test passes: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines && not (null normalized))

-- 测试 normalizeIndentation nested 失败用例
test_normalize_indentation_nested :: IO ()
test_normalize_indentation_nested = do
  let s = ""
  let nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s, "        }", "    }"]
  let normalized = U.normalizeIndentation nested
  let normLines = lines normalized
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "nested: " ++ show nested
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "Expected: length (filter (isPrefixOf \"    \") normLines) < length normLines && not (null normalized)"
  putStrLn $ "Actual: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines && not (null normalized))
  putStrLn $ "Test passes: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines && not (null normalized))

main :: IO ()
main = do
  putStrLn "=== Testing prop_normalize_indentation_tabs failure case ==="
  test_normalize_indentation_tabs
  putStrLn "\n=== Testing prop_remove_line_comments_multiline failure case ==="
  test_remove_line_comments_multiline
  putStrLn "\n=== Testing prop_is_complete_string_literal failure case ==="
  test_is_complete_string_literal
  putStrLn "\n=== Testing normalizeIndentation code block failure case ==="
  test_normalize_indentation_code_block
  putStrLn "\n=== Testing normalizeIndentation nested failure case ==="
  test_normalize_indentation_nested