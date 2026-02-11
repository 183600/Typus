import Utils (removeLineComments)
import Data.List (lines, isInfixOf)

-- Test the exact failing case and track all values
main :: IO ()
main = do
  putStrLn "=== Testing prop_remove_line_comments_multiline failure case ==="
  
  -- Test case 1: Empty strings (the failing case)
  let s1 = ""
      s2 = ""
      
  putStrLn "\n--- Test Case 1: Empty strings ---"
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  
  let valid1 = not ('\"' `elem` s1) && not ('\'' `elem` s1) && not ('\\' `elem` s1)
      valid2 = not ('\"' `elem` s2) && not ('\'' `elem` s2) && not ('\\' `elem` s2)
  
  putStrLn $ "valid1: " ++ show valid1
  putStrLn $ "valid2: " ++ show valid2
  putStrLn $ "valid1 && valid2: " ++ show (valid1 && valid2)
  
  if not (valid1 && valid2)
    then putStrLn "Skipped due to invalid characters"
    else do
      let line1 = s1 ++ "// comment1"
          line2 = s2 ++ "// comment2"
          multiline = line1 ++ "\n" ++ line2
          
      putStrLn $ "line1: " ++ show line1
      putStrLn $ "line2: " ++ show line2
      putStrLn $ "multiline: " ++ show multiline
      
      let result = removeLineComments multiline
          linesResult = lines result
          hasContent = any (not . null) [s1, s2]
          
      putStrLn $ "result: " ++ show result
      putStrLn $ "linesResult: " ++ show linesResult
      putStrLn $ "length linesResult: " ++ show (length linesResult)
      putStrLn $ "hasContent: " ++ show hasContent
      putStrLn $ "result == \"\\n\": " ++ show (result == "\n")
      
      if hasContent
        then do
          putStrLn $ "length linesResult >= 1: " ++ show (length linesResult >= 1)
          putStrLn $ "any (\"//\" `isInfixOf`) linesResult: " ++ show (any ("//" `isInfixOf`) linesResult)
          putStrLn $ "Test condition: " ++ show (length linesResult >= 1 && not (any ("//" `isInfixOf`) linesResult))
        else do
          putStrLn $ "Test condition (empty case): " ++ show (result == "\n")
  
  -- Test case 2: Non-empty strings to see the difference
  putStrLn "\n--- Test Case 2: Non-empty strings ---"
  let s1' = "code1"
      s2' = "code2"
      
  putStrLn $ "s1': " ++ show s1'
  putStrLn $ "s2': " ++ show s2'
  
  let valid1' = not ('\"' `elem` s1') && not ('\'' `elem` s1') && not ('\\' `elem` s1')
      valid2' = not ('\"' `elem` s2') && not ('\'' `elem` s2') && not ('\\' `elem` s2')
  
  if not (valid1' && valid2')
    then putStrLn "Skipped due to invalid characters"
    else do
      let line1' = s1' ++ "// comment1"
          line2' = s2' ++ "// comment2"
          multiline' = line1' ++ "\n" ++ line2'
          
      putStrLn $ "line1': " ++ show line1'
      putStrLn $ "line2': " ++ show line2'
      putStrLn $ "multiline': " ++ show multiline'
      
      let result' = removeLineComments multiline'
          linesResult' = lines result'
          hasContent' = any (not . null) [s1', s2']
          
      putStrLn $ "result': " ++ show result'
      putStrLn $ "linesResult': " ++ show linesResult'
      putStrLn $ "length linesResult': " ++ show (length linesResult')
      putStrLn $ "hasContent': " ++ show hasContent'
      
      if hasContent'
        then do
          putStrLn $ "length linesResult' >= 1: " ++ show (length linesResult' >= 1)
          putStrLn $ "any (\"//\" `isInfixOf`) linesResult': " ++ show (any ("//" `isInfixOf`) linesResult')
          putStrLn $ "Test condition: " ++ show (length linesResult' >= 1 && not (any ("//" `isInfixOf`) linesResult'))
        else do
          putStrLn $ "Test condition (empty case): " ++ show (result' == "\n")