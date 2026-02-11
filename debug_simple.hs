import Utils (removeLineComments)
import Data.List (lines, unlines)

-- Test the [String] version of the test
main :: IO ()
main = do
  putStrLn "=== Testing [String] version of prop_remove_line_comments_multiline ==="
  
  -- Test case 1: The failing case ["",""]
  let lines' = ["",""]
      
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
  
  let isNewlineCase = lines' == ["\n"]
      isEmptyCase = lines' == [""]
  
  putStrLn $ "lines' == [\"\\n\"]: " ++ show isNewlineCase
  putStrLn $ "lines' == [\"\"]: " ++ show isEmptyCase
  
  if isNewlineCase
    then do
      putStrLn $ "Test case 1: processed == \"\\n\": " ++ show (processed == "\n")
    else if isEmptyCase
      then do
        putStrLn $ "Test case 1: processed == \"\\n\": " ++ show (processed == "\n")
      else do
        putStrLn $ "Test case 1: length procLines === length lines': " ++ show (length procLines == length lines')
        putStrLn $ "Expected: " ++ show (length lines') ++ ", Got: " ++ show (length procLines)
