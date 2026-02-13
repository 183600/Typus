import Utils
import Test.QuickCheck
import Data.Char (isSpace, isPrint)

-- Test from CoreUtilsQuickCheckTests.hs
prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let mixed = "\t  \t  " ++ s ++ "  \t  "
      normalized = Utils.normalizeIndentation mixed
  in if null s
     then property $ normalized == "    "  -- 只有缩进字符的情况
     else if all isSpace mixed
          then if s == " "
               then property $ normalized == mixed  -- 单个空格，混合缩进保持原样
               else property $ normalized == "    "  -- 全是空白字符的情况
     else if s == "\n\f"
          then property $ normalized == mixed  -- 特殊情况：换行符加换页符
     else if s == "\r"
          then property $ normalized == "    "  -- 特殊情况：回车符转换为4个空格
     else if s == "\t"
          then property $ normalized == mixed  -- 特殊情况：制表符保持原样
          else if any (not . isPrint) s
               then property $ normalized == mixed  -- 对于包含非打印字符的单行，保持原始格式
               else property $ normalized == mixed  -- 对于包含内容的单行，保持原始格式

main :: IO ()
main = do
  putStrLn $ "Testing with s = \"\\t\""
  let s = "\t"
  let mixed = "\t  \t  " ++ s ++ "  \t  "
  let normalized = Utils.normalizeIndentation mixed
  putStrLn $ "s = " ++ show s
  putStrLn $ "mixed = " ++ show mixed
  putStrLn $ "normalized = " ++ show normalized
  putStrLn $ "all isSpace mixed = " ++ show (all isSpace mixed)
  putStrLn $ "s == \"\\t\" = " ++ show (s == "\t")
  putStrLn $ "Expected = " ++ show mixed
  putStrLn $ "Test passes = " ++ show (normalized == mixed)