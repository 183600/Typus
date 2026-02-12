import qualified Utils as U

-- 测试控制字符的情况
testControlChars :: IO ()
testControlChars = do
  let testCases = ["\STX", "\f", "\SOH", "\ETX"]
  mapM_ (\c -> do
    let input = "\t\t" ++ c ++ "\t"
        result = U.normalizeIndentation input
        expected = input  -- 期望保持原样
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Pass: " ++ show (result == expected)
    putStrLn "") testCases

main :: IO ()
main = testControlChars