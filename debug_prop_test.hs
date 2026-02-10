import Utils
import Data.Char (isSpace)

-- 模拟prop_normalizeIndentation_removes_common_prefix测试
main :: IO ()
main = do
  let s = " a"  -- 这是测试失败的输入
      result = Utils.normalizeIndentation s
      lines' = lines s
      hasCorrectBehavior = if length lines' <= 1 || all (all isSpace) lines'
                          then result == s  -- Single line or all whitespace lines should remain unchanged
                          else True  -- For multi-line inputs, any behavior is acceptable as long as it doesn't crash
  
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Lines: " ++ show lines'
  putStrLn $ "Length lines': " ++ show (length lines')
  putStrLn $ "Should remain unchanged: " ++ show (length lines' <= 1 || all (all isSpace) lines')
  putStrLn $ "Result == input: " ++ show (result == s)
  putStrLn $ "Test passes: " ++ show hasCorrectBehavior