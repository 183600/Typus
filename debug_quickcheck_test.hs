#!/usr/bin/env runhaskell

-- 导入模块
import Test.QuickCheck
import Utils (isCompleteStringLiteral)

-- 测试函数
prop_test :: String -> Property
prop_test s =
  let stringWithQuotes = "\"" ++ s ++ "\""
  in isCompleteStringLiteral stringWithQuotes === True

-- 主函数
main :: IO ()
main = do
  quickCheck prop_test
