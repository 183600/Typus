import qualified Utils as U
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  putStrLn "Debugging normalizeIndentation in detail:"
  
  -- Test case 1: prop_normalize_indentation_tabs with "a"
  putStrLn "\n=== Test case 1: prop_normalize_indentation_tabs with \"a\" ==="
  let input1 = "a"
  let withTabs1 = "\t\t" ++ input1 ++ "\t"
  let normalized1 = U.normalizeIndentation withTabs1
  putStrLn $ "Input string: " ++ show input1
  putStrLn $ "With tabs: " ++ show withTabs1
  putStrLn $ "Normalized: " ++ show normalized1
  putStrLn $ "Starts with \"\\t\\t\": " ++ show ("\t\t" `isPrefixOf` normalized1)
  
  -- Check individual conditions
  putStrLn $ "All spaces: " ++ show (all isSpace withTabs1)
  putStrLn $ "Starts with \t\t: " ++ show ("\t\t" `isPrefixOf` withTabs1)
  
  -- Test case 2: prop_normalize_indentation_multiline_mixed with [""]
  putStrLn "\n=== Test case 2: prop_normalize_indentation_multiline_mixed with [\"\"] ==="
  let input2 = [""]
  let withMixed2 = map ("\t  " ++) input2
  let unlines2 = unlines withMixed2
  let normalized2 = U.normalizeIndentation unlines2
  putStrLn $ "Input lines: " ++ show input2
  putStrLn $ "With mixed: " ++ show withMixed2
  putStrLn $ "Unlines: " ++ show unlines2
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Matches expected: " ++ show (normalized2 == "    ")
  
  -- Check if unlines2 matches the special case
  putStrLn $ "unlines2 == \"\\t  \\n\": " ++ show (unlines2 == "\t  \n")