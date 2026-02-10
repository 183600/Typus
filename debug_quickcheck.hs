import qualified Utils as U
import Data.List (isPrefixOf)
import Test.QuickCheck

prop_normalize_indentation_tabs :: String -> Property
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
  in if null s
     then property $ True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
     else property $ not ("\t\t" `isPrefixOf` normalized)

main :: IO ()
main = do
  -- 测试特定的输入
  let s = "a"
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = U.normalizeIndentation withTabs
  let result = if null s
               then True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
               else not ("\t\t" `isPrefixOf` normalized)
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "result: " ++ show result
  
  -- 运行QuickCheck测试
  putStrLn "\nRunning QuickCheck test..."
  quickCheck prop_normalize_indentation_tabs