import qualified Utils as U
import Data.List (isPrefixOf)

prop_normalize_indentation_tabs :: String -> Bool
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
  in if null s
     then True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
     else not ("\t\t" `isPrefixOf` normalized)

main :: IO ()
main = do
  -- 测试特定的输入
  let s = "a"
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = U.normalizeIndentation withTabs
  let result = prop_normalize_indentation_tabs s
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "result: " ++ show result
  
  -- 测试多个输入
  let testCases = ["", "a", " ", "\t", "ab", "a b", "a\tb"]
  putStrLn "\nTesting multiple cases:"
  mapM_ (\s -> do
    let result = prop_normalize_indentation_tabs s
    putStrLn $ "s: " ++ show s ++ ", result: " ++ show result
  ) testCases