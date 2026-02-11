import qualified Utils as U
import Data.List (isInfixOf)

-- 直接复制测试中的函数
prop_normalize_indentation_mixed :: String -> Bool
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = U.normalizeIndentation mixed
  in not ("\t" `isInfixOf` normalized)

main :: IO ()
main = do
  putStrLn "Testing specific case:"
  let testInput = "a"
  let mixed = "  \t  " ++ testInput
  let result = U.normalizeIndentation mixed
  let passes = not ("\t" `isInfixOf` result)
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Contains tab: " ++ show ("\t" `isInfixOf` result)
  putStrLn $ "Test passes: " ++ show passes
  putStrLn $ "Function returns: " ++ show (prop_normalize_indentation_mixed testInput)