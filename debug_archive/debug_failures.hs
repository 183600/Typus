import Utils
import Test.QuickCheck
import System.Random

-- 测试特定用例
testSpecificCase :: IO ()
testSpecificCase = do
  -- 测试失败的具体输入：["", ""]
  let lines' = ["", ""]
  let code = unlines lines'
  let processed = removeLineComments code
  let procLines = lines processed
  
  putStrLn $ "Input lines': " ++ show lines'
  putStrLn $ "Code: " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Expected length: 2, Actual length: " ++ show (length procLines)
  putStrLn $ "Test result: " ++ show (length procLines == 2)
  putStrLn ""
  
  -- 检查unlines的行为
  putStrLn $ "unlines [\"\"] = " ++ show (unlines [""])
  putStrLn $ "lines (unlines [\"\"]) = " ++ show (lines (unlines [""]))
  putStrLn $ "length (lines (unlines [\"\"])) = " ++ show (length (lines (unlines [""])))
  putStrLn ""
  
  putStrLn $ "unlines [\"\",\"\"] = " ++ show (unlines ["", ""])
  putStrLn $ "lines (unlines [\"\",\"\"]) = " ++ show (lines (unlines ["", ""]))
  putStrLn $ "length (lines (unlines [\"\",\"\"])) = " ++ show (length (lines (unlines ["", ""])))
  putStrLn ""

-- 测试字符串字面量
testStringLiteral :: IO ()
testStringLiteral = do
  let s = ""
  let withBackslash = "\"" ++ s ++ "\\\\\""
  let result = isCompleteStringLiteral withBackslash
  
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "With backslash: " ++ show withBackslash
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: True"
  putStrLn $ "Test result: " ++ show (result == True)
  putStrLn ""
  
  -- 手动测试失败案例
  let failureCase = "\\"
  let result2 = isCompleteStringLiteral failureCase
  putStrLn $ "Failure case: " ++ show failureCase
  putStrLn $ "Result: " ++ show result2
  putStrLn ""

main :: IO ()
main = do
  putStrLn "=== Testing prop_remove_line_comments_multiline failure case ==="
  testSpecificCase
  
  putStrLn "=== Testing prop_is_complete_string_literal_escape_backslash failure case ==="
  testStringLiteral