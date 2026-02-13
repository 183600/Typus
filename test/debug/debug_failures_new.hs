import qualified Utils as U

-- 测试失败的情况
testFailures :: IO ()
testFailures = do
  -- 测试1: prop_normalize_indentation_tabs 输入为 "\n"
  let s1 = "\n"
      withTabs1 = "\t\t" ++ s1 ++ "\t"
      normalized1 = U.normalizeIndentation withTabs1
  putStrLn $ "Test 1 - Input s: " ++ show s1
  putStrLn $ "WithTabs: " ++ show withTabs1
  putStrLn $ "Normalized: " ++ show normalized1
  putStrLn $ "Expected (should be same as withTabs): " ++ show withTabs1
  putStrLn $ "Pass: " ++ show (normalized1 == withTabs1)
  putStrLn ""

  -- 测试2: prop_normalize_indentation_mixed 输入为 "\t"
  let s2 = "\t"
      mixed2 = "\t  \t  " ++ s2 ++ "  \t  "
      normalized2 = U.normalizeIndentation mixed2
  putStrLn $ "Test 2 - Input s: " ++ show s2
  putStrLn $ "Mixed: " ++ show mixed2
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Expected (should be same as mixed): " ++ show mixed2
  putStrLn $ "Pass: " ++ show (normalized2 == mixed2)
  putStrLn ""

  -- 测试3: prop_normalize_indentation_multiline_mixed 输入为 ["a","\n"]
  let lines3 = ["a", "\n"]
      withMixed3 = map ("\t  " ++) lines3
      normalized3 = U.normalizeIndentation (unlines withMixed3)
  putStrLn $ "Test 3 - Input lines: " ++ show lines3
  putStrLn $ "WithMixed: " ++ show (unlines withMixed3)
  putStrLn $ "Normalized: " ++ show normalized3
  putStrLn $ "Expected (should be same as withMixed): " ++ show (unlines withMixed3)
  putStrLn $ "Pass: " ++ show (normalized3 == unlines withMixed3)

main :: IO ()
main = testFailures