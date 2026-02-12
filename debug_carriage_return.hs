import qualified Utils as U

-- 测试 "\r" 的情况
testCarriageReturn :: IO ()
testCarriageReturn = do
  let input = "\r"
      normalized = U.normalizeIndentation input
      expected = "    "
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Pass: " ++ show (normalized == expected)

main :: IO ()
main = testCarriageReturn