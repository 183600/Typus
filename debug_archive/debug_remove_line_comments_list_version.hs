import Utils (removeLineComments)
import Data.List (lines, unlines)

-- Test the [String] version of the test
main :: IO ()
main = do
  putStrLn "=== Testing [String] version of prop_remove_line_comments_multiline ==="
  
  -- Test case 1: The failing case [""]
  let lines' = [""]
      
  putStrLn $ "\n--- Test Case 1: " ++ show lines' ++ " ---"
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "length lines': " ++ show (length lines')
  
  let code = unlines lines'
      processed = removeLineComments code
      procLines = lines processed
      
  putStrLn $ "code (unlines lines'): " ++ show code
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "procLines: " ++ show procLines
  putStrLn $ "length procLines: " ++ show (length procLines)
  
putStrLn $ "lines' == ["\\n"]: " ++ show (lines' == ["\n"])
  putStrLn $ "lines' == [""]: " ++ show (lines' == [""])
  
  if lines' == ["\n"]
    then do
      putStrLn $ "Test case 1: processed == ""\n"": " ++ show (processed == "\n")
    else if lines' == [""]
      then do
        putStrLn $ "Test case 1: processed == ""\n"": " ++ show (processed == "\n")
      else do
        putStrLn $ "Test case 1: length procLines === length lines': " ++ show (length procLines == length lines')
        putStrLn $ "Expected: " ++ show (length lines') ++ ", Got: " ++ show (length procLines)
  
  -- Test case 2: Single empty line
  let lines2 = [""]
      
  putStrLn $ "\n--- Test Case 2: " ++ show lines2 ++ " ---"
  putStrLn $ "lines2: " ++ show lines2
  putStrLn $ "length lines2: " ++ show (length lines2)
  
  let code2 = unlines lines2
      processed2 = removeLineComments code2
      procLines2 = lines processed2
      
  putStrLn $ "code2 (unlines lines2): " ++ show code2
  putStrLn $ "processed2: " ++ show processed2
  putStrLn $ "procLines2: " ++ show procLines2
  putStrLn $ "length procLines2: " ++ show (length procLines2)
  
  if lines2 == ["\n"]
    then do
      putStrLn $ "Test case 2: processed2 == ""\n"": " ++ show (processed2 == "\n")
    else if lines2 == [""]
      then do
        putStrLn $ "Test case 2: processed2 == ""\n"": " ++ show (processed2 == "\n")
      else do
        putStrLn $ "Test case 2: length procLines2 === length lines2: " ++ show (length procLines2 == length lines2)
  
  -- Test case 3: Single newline
  let lines3 = ["\n"]
      
  putStrLn $ "\n--- Test Case 3: " ++ show lines3 ++ " ---"
  putStrLn $ "lines3: " ++ show lines3
  putStrLn $ "length lines3: " ++ show (length lines3)
  
  let code3 = unlines lines3
      processed3 = removeLineComments code3
      procLines3 = lines processed3
      
  putStrLn $ "code3 (unlines lines3): " ++ show code3
  putStrLn $ "processed3: " ++ show processed3
  putStrLn $ "procLines3: " ++ show procLines3
  putStrLn $ "length procLines3: " ++ show (length procLines3)
  
  if lines3 == ["\n"]
    then do
      putStrLn $ "Test case 3: processed3 == ""\n"": " ++ show (processed3 == "\n")
    else if lines3 == [""]
      then do
        putStrLn $ "Test case 3: processed3 == ""\n"": " ++ show (processed3 == "\n")
      else do
        putStrLn $ "Test case 3: length procLines3 === length lines3: " ++ show (length procLines3 == length lines3)