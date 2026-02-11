import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- 使用测试失败的确切输入
  let s = "a"
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = Utils.normalizeIndentation withTabs
  
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With tabs: " ++ show withTabs
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Starts with tabs: " ++ show ("\t\t" `isPrefixOf` normalized)
  
  -- 测试条件
  let testResult = if null s
                  then True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
                  else not ("\t\t" `isPrefixOf` normalized)
  
  putStrLn $ "Test passes: " ++ show testResult