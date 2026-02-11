import Utils
import Data.List
import Data.Char (isSpace)

-- 测试 prop_remove_line_comments_multiline
test_remove_line_comments_multiline :: [String] -> IO ()
test_remove_line_comments_multiline lines' = do
  let code = unlines lines'
      processed = removeLineComments code
      procLines = lines processed
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Code: " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Length check: " ++ show (length procLines) ++ " vs " ++ show (length lines')
  if lines' == ["\n"]
    then putStrLn $ "Special case: " ++ show (processed == "\n")
    else if lines' == [""]
         then putStrLn $ "Empty case: " ++ show (processed == "\n")
         else putStrLn $ "Normal case: " ++ show (length procLines == length lines')
  putStrLn ""

-- 测试 prop_remove_line_comments_string_slash
test_remove_line_comments_string_slash :: String -> IO ()
test_remove_line_comments_string_slash s = do
  let withSlash = "\"" ++ s ++ "// not comment\""
      processed = removeLineComments withSlash
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With slash: " ++ show withSlash
  putStrLn $ "Processed: " ++ show processed
  if s == "\n"
    then putStrLn $ "Newline case: " ++ show (processed == "\"\n// not comment\"")
    else if s == ""
         then putStrLn $ "Empty case: " ++ show (processed == "\"// not comment\"")
         else putStrLn $ "Normal case: " ++ show ("// not comment" `isInfixOf` processed)
  putStrLn ""

-- 测试 prop_remove_line_comments_end
test_remove_line_comments_end :: String -> IO ()
test_remove_line_comments_end s = do
  let withComment = s ++ "// comment"
      processed = removeLineComments withComment
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With comment: " ++ show withComment
  putStrLn $ "Processed: " ++ show processed
  if s == "'"
    then putStrLn $ "Quote case: " ++ show (processed == "'// comment")
    else if length s == 1 && all isSpace s
         then putStrLn $ "Space case: " ++ show (processed == s)
         else if s == "/"
              then putStrLn $ "Slash case: " ++ show (processed == "")
              else putStrLn $ "Normal case: " ++ show (processed == s)
  putStrLn ""

-- 测试 prop_remove_comments_single_line
test_remove_comments_single_line :: String -> IO ()
test_remove_comments_single_line s = do
  let withSingle = "//" ++ s
      processed = removeComments withSingle
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With single: " ++ show withSingle
  putStrLn $ "Processed: " ++ show processed
  if s == "\n"
    then putStrLn $ "Newline case: " ++ show (processed == "\n")
    else if s == "\""
         then putStrLn $ "Quote case: " ++ show (processed == "\"")
         else if s == "a\n"
              then putStrLn $ "Char+newline case: " ++ show (processed == "a\n")
              else if s == "\na"
                   then putStrLn $ "Newline+char case: " ++ show (processed == "\na")
                   else putStrLn $ "Normal case: " ++ show (null processed)
  putStrLn ""

-- 测试 prop_normalize_indentation_mixed
test_normalize_indentation_mixed :: String -> IO ()
test_normalize_indentation_mixed s = do
  let mixed = "\t  \t  " ++ s ++ "  \t  "
      normalized = normalizeIndentation mixed
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Normalized: " ++ show normalized
  if null s
    then putStrLn $ "Empty case: " ++ show (normalized == "    ")
    else if all isSpace mixed
         then putStrLn $ "All space case: " ++ show (normalized == "    ")
         else putStrLn $ "Normal case: " ++ show (normalized == mixed)
  putStrLn ""

-- 测试 normalizeIndentation code block
test_normalize_indentation_code_block :: IO ()
test_normalize_indentation_code_block = do
  let codeBlock = "    if True\n        then putStrLn \"Yes\"\n        else putStrLn \"No\""
      normalized = normalizeIndentation codeBlock
  putStrLn $ "Code block: " ++ show codeBlock
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn ""

-- 测试 normalizeIndentation nested
test_normalize_indentation_nested :: IO ()
test_normalize_indentation_nested = do
  let nested = "    function outer() {\n        function inner() {\n            return 42;\n        }\n    }"
      normalized = normalizeIndentation nested
  putStrLn $ "Nested: " ++ show nested
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn ""

-- 测试 normalizeIndentation labels
test_normalize_indentation_labels :: IO ()
test_normalize_indentation_labels = do
  let labeled = "label1:\n    goto label1;\nlabel2:\n    goto label2;"
      normalized = normalizeIndentation labeled
  putStrLn $ "Labeled: " ++ show labeled
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeLineComments multiline:"
  test_remove_line_comments_multiline ["\n"]
  test_remove_line_comments_multiline [""]
  test_remove_line_comments_multiline ["hello", "world"]
  
  putStrLn "Testing removeLineComments string slash:"
  test_remove_line_comments_string_slash "\n"
  test_remove_line_comments_string_slash ""
  test_remove_line_comments_string_slash "test"
  
  putStrLn "Testing removeLineComments end:"
  test_remove_line_comments_end "'"
  test_remove_line_comments_end " "
  test_remove_line_comments_end "/"
  test_remove_line_comments_end "test"
  
  putStrLn "Testing removeComments single line:"
  test_remove_comments_single_line "\n"
  test_remove_comments_single_line "\""
  test_remove_comments_single_line "a\n"
  test_remove_comments_single_line "\na"
  test_remove_comments_single_line "test"
  
  putStrLn "Testing normalizeIndentation mixed:"
  test_normalize_indentation_mixed ""
  test_normalize_indentation_mixed "test"
  test_normalize_indentation_mixed "  "
  
  putStrLn "Testing normalizeIndentation special cases:"
  test_normalize_indentation_code_block
  test_normalize_indentation_nested
  test_normalize_indentation_labels