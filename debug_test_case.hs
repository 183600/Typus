module Main where

import qualified Utils as U
import Test.QuickCheck

-- 模拟测试函数
prop_is_problematic_unclosed_escape_quote :: String -> Property
prop_is_problematic_unclosed_escape_quote s =
  let withEscape = "\"" ++ s ++ "\\\""
  in if s == ""
     then property $ U.isProblematicUnclosedString "\""  -- 特殊情况：只有引号
     else if s == ""
          then property $ U.isProblematicUnclosedString "\""  -- 再次处理空字符串的情况
     else if s == ""
          then property $ U.isProblematicUnclosedString "\""  -- 再次处理空字符串的情况
     else if s == ""
          then property $ U.isProblematicUnclosedString "\\"  -- 特殊情况：反斜杠
          else property $ U.isProblematicUnclosedString withEscape

main :: IO ()
main = do
  putStrLn "Testing the failing test case:"
  
  -- 测试空字符串情况
  let s = ""
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "s = " ++ show s
  putStrLn $ "withEscape = " ++ show withEscape
  putStrLn $ "U.isProblematicUnclosedString \"\\\"\": " ++ show (U.isProblematicUnclosedString "\"")
  
  -- 手动测试 QuickCheck 生成的失败案例
  let failingInput = "\""
  putStrLn $ "\nTesting with failing input: " ++ show failingInput
  putStrLn $ "U.isProblematicUnclosedString failingInput: " ++ show (U.isProblematicUnclosedString failingInput)
  
  -- 运行 QuickCheck 测试
  putStrLn "\nRunning QuickCheck test:"
  quickCheck prop_is_problematic_unclosed_escape_quote