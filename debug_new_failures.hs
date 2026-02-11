import qualified Utils as U
import Data.List (isPrefixOf)

-- 测试 prop_normalize_indentation_tabs 失败用例
test_normalize_indentation_tabs :: IO ()
test_normalize_indentation_tabs = do
  let s = "\1013245"
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = U.normalizeIndentation withTabs
  putStrLn $ "Input: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Expected: not (\"\\t\\t\" `isPrefixOf` normalized)"
  putStrLn $ "Actual: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))
  
  -- 测试 normalizeIndentation 的行为
  putStrLn "\n=== Testing normalizeIndentation with tabs ==="
  let testInput = "\t\t\1013245\t"
  let testOutput = U.normalizeIndentation testInput
  putStrLn $ "Test input: " ++ show testInput
  putStrLn $ "Test output: " ++ show testOutput
  putStrLn $ "Has tabs at start: " ++ show ("\t\t" `isPrefixOf` testOutput)

-- 测试 prop_normalize_indentation_mixed 失败用例
test_normalize_indentation_mixed :: IO ()
test_normalize_indentation_mixed = do
  let s = "a"
  let mixed = "\t  \t  " ++ s ++ "  \t  "
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "\nInput: " ++ show s
  putStrLn $ "mixed: " ++ show mixed
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual: " ++ show normalized
  putStrLn $ "Test passes: " ++ show (normalized == "    ")

main :: IO ()
main = do
  putStrLn "=== Testing prop_normalize_indentation_tabs failure case ==="
  test_normalize_indentation_tabs
  test_normalize_indentation_mixed