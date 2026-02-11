#!/usr/bin/env stack
-- stack runghc --package QuickCheck

-- 模拟 removeSingleLineComments 函数
removeSingleLineComments :: String -> String
removeSingleLineComments [] = []
removeSingleLineComments ('"':xs) = '"' : removeSingleLineComments xs
removeSingleLineComments ('\'':xs) = '\'' : removeSingleLineComments xs
removeSingleLineComments ('/':'/':xs) = []
removeSingleLineComments ('\n':cs) = '\n' : removeSingleLineComments cs
removeSingleLineComments (c:cs) = c : removeSingleLineComments cs

-- 测试 removeSingleLineComments 对空字符串的处理
test_remove_single_line_comments :: IO ()
test_remove_single_line_comments = do
  let line = ""
  let processed = removeSingleLineComments line
  putStrLn $ "Input line: " ++ show line
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Null: " ++ show (null processed)
  putStrLn ""

-- 测试 processLine 函数
processLine :: String -> String
processLine line = 
  if null line
    then line  -- 空行保持不变
    else removeSingleLineComments line

test_process_line :: IO ()
test_process_line = do
  let line = ""
  let processed = processLine line
  putStrLn $ "Input line: " ++ show line
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Null: " ++ show (null processed)
  putStrLn ""

-- 测试整个流程
test_full_flow :: IO ()
test_full_flow = do
  let input = "\n\n"
  let inputLines = lines input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input lines: " ++ show inputLines
  putStrLn $ "Length of input lines: " ++ show (length inputLines)
  
  let processedLines = map processLine inputLines
  let result = unlines processedLines
  putStrLn $ "Processed lines: " ++ show processedLines
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Result lines: " ++ show (lines result)
  putStrLn $ "Length of result lines: " ++ show (length (lines result))
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeSingleLineComments:"
  test_remove_single_line_comments
  
  putStrLn "Testing processLine:"
  test_process_line
  
  putStrLn "Testing full flow:"
  test_full_flow