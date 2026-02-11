import Utils

-- Test failing cases
main :: IO ()
main = do
  putStrLn "=== Testing remove_line_comments_multiline failures ==="
  let test1 = "b\n"
  let result1 = removeLineComments test1
  putStrLn $ "Input: " ++ show test1
  putStrLn $ "Output: " ++ show result1
  putStrLn $ "Output lines: " ++ show (lines result1)
  putStrLn $ "Number of lines: " ++ show (length (lines result1))
  putStrLn ""
  
  putStrLn "=== Testing remove_comments_single_line failures ==="
  let test2 = "b\n"
  let result2 = removeComments test2
  putStrLn $ "Input: " ++ show test2
  putStrLn $ "Output: " ++ show result2
  putStrLn ""
  
  putStrLn "=== Testing normalize_indentation_empty_lines failures ==="
  let test3a = " "
  let result3a = normalizeIndentation test3a
  putStrLn $ "Input: " ++ show test3a
  putStrLn $ "Output: " ++ show result3a
  putStrLn ""
  
  let test3b = "\v"
  let result3b = normalizeIndentation test3b
  putStrLn $ "Input: " ++ show test3b
  putStrLn $ "Output: " ++ show result3b
  putStrLn ""
  
  putStrLn "=== Testing normalize_indentation_multiline_mixed failures ==="
  let test4 = ["", ""]
  let result4 = normalizeIndentation (unlines test4)
  putStrLn $ "Input: " ++ show test4
  putStrLn $ "Output: " ++ show result4
  putStrLn $ "Output lines: " ++ show (lines result4)
  putStrLn $ "Number of lines: " ++ show (length (lines result4))
  putStrLn ""
  
  putStrLn "=== Testing remove_line_comments_end failures ==="
  let test5 = "a'"
  let result5 = removeLineComments test5
  putStrLn $ "Input: " ++ show test5
  putStrLn $ "Output: " ++ show result5
  putStrLn ""