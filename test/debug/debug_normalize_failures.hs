import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation failures:"
  
  -- Test case 1: prop_normalize_indentation_tabs with "a"
  putStrLn "\n=== Test case 1: prop_normalize_indentation_tabs with \"a\" ==="
  let input1 = "\t\ta\t"
  let result1 = normalizeIndentation input1
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Starts with \"\\t\\t\": " ++ show ("\t\t" `isPrefixOf` result1)
  putStrLn $ "Expected: Should NOT start with \"\\t\\t\""
  
  -- Test case 2: prop_normalize_indentation_multiline_mixed with [""]
  putStrLn "\n=== Test case 2: prop_normalize_indentation_multiline_mixed with [\"\"] ==="
  let input2 = unlines (map ("\t  " ++) [""])
  let result2 = normalizeIndentation input2
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Expected: \"    \" (4 spaces)"
  putStrLn $ "Actual matches expected: " ++ show (result2 == "    ")