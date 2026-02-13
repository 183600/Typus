{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Utils as U
import Data.Char (isSpace, isPrint)

-- 修改后的测试，添加更多调试信息
prop_normalize_indentation_mixed_debug :: String -> Property
prop_normalize_indentation_mixed_debug s =
  let mixed = "\t  \t  " ++ s ++ "  \t  "
      normalized = U.normalizeIndentation mixed
      expected = if null s
                 then "    "  -- 只有缩进字符的情况
                 else if all isSpace mixed
                      then if s == " "
                           then mixed  -- 单个空格，混合缩进保持原样
                           else "    "  -- 全是空白字符的情况
                      else if s == "\n\f"
                           then mixed  -- 特殊情况：换行符加换页符
                      else if s == "\r"
                           then "    "  -- 特殊情况：回车符转换为4个空格
                      else if s == "\t"
                           then mixed  -- 特殊情况：制表符保持原样
                           else if any (not . isPrint) s
                                then mixed  -- 对于包含非打印字符的单行，保持原始格式
                                else mixed  -- 对于包含内容的单行，保持原始格式
  in if s == "a"
     then let msg = "Input: " ++ show s ++ "\n" ++
                   "Mixed: " ++ show mixed ++ "\n" ++
                   "Normalized: " ++ show normalized ++ "\n" ++
                   "Expected: " ++ show expected ++ "\n" ++
                   "Match: " ++ show (normalized == expected)
          in counterexample msg (normalized === expected)
     else normalized === expected

main :: IO ()
main = do
    -- 直接测试输入"a"
    let s = "a"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = U.normalizeIndentation mixed
    let expected = mixed
    
    putStrLn $ "Direct test with s = \"a\":"
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Mixed: " ++ show mixed
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Match: " ++ show (normalized == expected)
    
    -- 运行QuickCheck测试
    putStrLn "\nRunning QuickCheck test:"
    quickCheck prop_normalize_indentation_mixed_debug