import qualified Utils as U

-- 测试 prop_normalize_indentation_mixed 的情况
testMixedIndentation :: IO ()
testMixedIndentation = do
  let s = "\f"
      mixed = "\t  \t  " ++ s ++ "  \t  "
      normalized = U.normalizeIndentation mixed
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected (should be same as mixed): " ++ show mixed
  putStrLn $ "Pass: " ++ show (normalized == mixed)

main :: IO ()
main = testMixedIndentation