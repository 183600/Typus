import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test case 1: normalize indentation tabs with "a"
  let test1 = "\t\ta\t"
      result1 = normalizeIndentation test1
  putStrLn $ "Test 1 - Input: " ++ show test1
  putStrLn $ "Test 1 - Output: " ++ show result1
  putStrLn $ "Test 1 - Starts with tabs: " ++ show ("\t\t" `isPrefixOf` result1)
  putStrLn ""
  
  -- Test case 2: normalizeIndentation code block with empty string
  let test2 = unlines ["    if condition {", "        // do something", "        return " ++ "", "    }"]
      result2 = normalizeIndentation test2
      result2Lines = lines result2
      nonCommentLines2 = filter (not . isPrefixOf "//") result2Lines
  putStrLn $ "Test 2 - Input: " ++ show test2
  putStrLn $ "Test 2 - Output: " ++ show result2
  putStrLn $ "Test 2 - Non-comment lines start with spaces: " ++ show (any (isPrefixOf "    ") nonCommentLines2)
  putStrLn ""
  
  -- Test case 3: normalizeIndentation nested with empty string
  let test3 = unlines ["    func outer() {", "        func inner() {", "            " ++ "", "        }", "    }"]
      result3 = normalizeIndentation test3
      result3Lines = lines result3
  putStrLn $ "Test 3 - Input: " ++ show test3
  putStrLn $ "Test 3 - Output: " ++ show result3
  putStrLn $ "Test 3 - Lines start with spaces: " ++ show (any (isPrefixOf "    ") result3Lines)
  putStrLn ""
  
  -- Test case 4: normalizeIndentation labels with empty string
  let test4 = unlines ["label1:", "    " ++ "", "label2:", "    " ++ ""]
      result4 = normalizeIndentation test4
      result4Lines = lines result4
  putStrLn $ "Test 4 - Input: " ++ show test4
  putStrLn $ "Test 4 - Output: " ++ show result4
  putStrLn $ "Test 4 - Lines start with spaces: " ++ show (any (isPrefixOf "    ") result4Lines)