import qualified Utils as U
import Data.List (isInfixOf)

-- 直接复制测试中的函数，完全相同的逻辑
prop_normalize_indentation_mixed :: String -> Bool
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = U.normalizeIndentation mixed
  in not ("\t" `isInfixOf` normalized)

main :: IO ()
main = do
  putStrLn "Detailed testing of reported failure case:"
  let testInput = "a"
  let mixed = "  \t  " ++ testInput
  let result = U.normalizeIndentation mixed
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Result chars: " ++ show (map (\c -> if c == '\t' then "TAB" else show c) result)
  putStrLn $ "Contains tab: " ++ show ("\t" `isInfixOf` result)
  putStrLn $ "Test result: " ++ show (prop_normalize_indentation_mixed testInput)
  
  putStrLn "\nTesting case with empty string:"
  let testInput2 = ""
  let mixed2 = "  \t  " ++ testInput2
  let result2 = U.normalizeIndentation mixed2
  putStrLn $ "Input: " ++ show testInput2
  putStrLn $ "Mixed: " ++ show mixed2
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Contains tab: " ++ show ("\t" `isInfixOf` result2)
  putStrLn $ "Test result: " ++ show (prop_normalize_indentation_mixed testInput2)