import qualified Utils as U

-- 测试 "\t" 的情况
testTab :: IO ()
testTab = do
  let input = "\t"
      normalized = U.normalizeIndentation input
      expected = "    "
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Pass: " ++ show (normalized == expected)

main :: IO ()
main = testTab