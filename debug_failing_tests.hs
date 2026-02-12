import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn "=== Debug failing tests ==="
  
  -- Test case 1: prop_normalize_indentation_tabs with "a"
  putStrLn "\nTest case 1: prop_normalize_indentation_tabs with \"a\""
  let input1 = "a"
  let withTabs1 = "\t\t" ++ input1 ++ "\t"
  let normalized1 = normalizeIndentation withTabs1
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "With tabs: " ++ show withTabs1
  putStrLn $ "Normalized: " ++ show normalized1
  putStrLn $ "Expected: not (\"\\t\\t\" `isPrefixOf` normalized)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` normalized1))
  
  -- Test case 2: prop_normalize_indentation_multiline_mixed with [""]
  putStrLn "\nTest case 2: prop_normalize_indentation_multiline_mixed with [\"\"]"
  let input2 = [""]
  let withMixed2 = map ("\t  " ++) input2
  let normalized2 = normalizeIndentation (unlines withMixed2)
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "With mixed: " ++ show withMixed2
  putStrLn $ "Unlines with mixed: " ++ show (unlines withMixed2)
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual result: " ++ show (normalized2 == "    ")