import qualified Utils as U
import Data.List (isInfixOf)

-- 直接复制测试中的函数，完全相同的逻辑
prop_normalize_indentation_mixed :: String -> Bool
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = U.normalizeIndentation mixed
  in not ("\t" `isInfixOf` normalized)

-- 测试多个输入，包括 QuickCheck 可能生成的各种情况
testAllCases :: IO ()
testAllCases = do
  putStrLn "Testing various cases that QuickCheck might generate:"
  
  let testCases = ["a", "", "b", "c", "hello", "x", "y", "z", "1", "2", "!", "@", "\n", "\t", " ", "ab", "abc"]
  
  mapM_ (\testInput -> do
    let mixed = "  \t  " ++ testInput
    let result = U.normalizeIndentation mixed
    let passes = not ("\t" `isInfixOf` result)
    putStrLn $ "Input: " ++ show testInput ++ ", Result: " ++ show result ++ ", Passes: " ++ show passes
    if not passes
      then putStrLn $ "*** FAILURE: Input " ++ show testInput ++ " produces result with tabs!"
      else putStrLn ""
  ) testCases

-- 详细测试失败案例
testFailureCase :: IO ()
testFailureCase = do
  putStrLn "\nDetailed testing of reported failure case:"
  let testInput = "a"
  let mixed = "  \t  " ++ testInput
  let result = U.normalizeIndentation mixed
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Result chars: " ++ show (map (\c -> if c == '\t' then "TAB" else show c) result)
  putStrLn $ "Contains tab: " ++ show ("\t" `isInfixOf` result)
  putStrLn $ "Test result: " ++ show (prop_normalize_indentation_mixed testInput)

main :: IO ()
main = do
  testFailureCase
  testAllCases