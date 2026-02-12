import Utils
import Test.QuickCheck

-- Test the property that's failing
prop_normalize_indentation_tabs :: String -> Bool
prop_normalize_indentation_tabs input = 
  -- The test is failing for input = "\t"
  if input == "\t"
    then normalizeIndentation input /= "\t"  -- It should not equal "\t"
    else True  -- For other inputs, we don't care in this test

main :: IO ()
main = do
    let input = "\t"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Result == \"\\t\": " ++ show (result == "\t")
    putStrLn $ "Result /= \"\\t\": " ++ show (result /= "\t")
    putStrLn $ "Property passes: " ++ show (prop_normalize_indentation_tabs input)