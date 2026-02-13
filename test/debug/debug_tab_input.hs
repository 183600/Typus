import qualified Utils as U

-- 测试输入为 "\t" 的情况
testTabInput :: IO ()
testTabInput = do
  let s = "\t"
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "WithTabs: " ++ show withTabs
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected (should be same as withTabs): " ++ show withTabs
  putStrLn $ "Pass: " ++ show (normalized == withTabs)

main :: IO ()
main = testTabInput