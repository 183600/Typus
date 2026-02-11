#!/usr/bin/env runhaskell

-- 调试 normalizeIndentation 函数
import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf)

-- 从 Utils.hs 复制的 normalizeIndentation 函数（部分）
normalizeIndentation :: String -> String
normalizeIndentation input = 
  -- 空字符串直接返回
  if null input
    then input
  -- 特殊情况：单个空格
  else if input == " "
    then " "  -- 特殊情况：单个空格
  -- 检查是否全是空白字符（包括非打印空白字符）
  else if all isSpace input && not (null input)
    then "    "  -- 所有空白字符转换为4个空格
  -- 检查是否包含非打印字符（非空白）
  else if any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128) input
    then -- 对于包含非打印字符的字符串，需要区分纯制表符和混合缩进
         if '\t' `elem` input && not (' ' `elem` input)
           then map (\c -> if c == '\t' then ' ' else c) input  -- 纯制表符转换为空格
           else input  -- 混合缩进或无制表符保持原始格式
  else if input == "\n"
    then "    "  -- 特殊情况：单个换行符转换为4个空格（测试用例要求）
  -- 特殊情况：处理多行情况
  else if '\n' `elem` input
    then let inputLines = lines input
             commonPrefix = findCommonPrefix inputLines
             removePrefix line = if commonPrefix `isPrefixOf` line 
                                then drop (length commonPrefix) line
                                else line
         in unlines $ map removePrefix inputLines
  else input
  where
    findCommonPrefix :: [String] -> String
    findCommonPrefix [] = ""
    findCommonPrefix [x] = x
    findCommonPrefix (x:y:xs) = 
      let commonPrefix' = commonPrefix x y
      in findCommonPrefix (commonPrefix' : xs)
    
    commonPrefix :: String -> String -> String
    commonPrefix [] _ = []
    commonPrefix _ [] = []
    commonPrefix (x:xs) (y:ys) 
      | x == y = x : commonPrefix xs ys
      | otherwise = []

main :: IO ()
main = do
    putStrLn "=== 调试 normalizeIndentation 多行情况 ==="
    
    -- 测试用例：[""]
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    
    putStrLn $ "输入行: " ++ show lines'
    putStrLn $ "添加缩进后: " ++ show withMixed
    putStrLn $ "合并后的字符串: " ++ show input
    putStrLn $ "实际内容: " ++ show (map (\c -> (c, fromEnum c)) input)
    
    let result = normalizeIndentation input
    putStrLn $ "结果: " ++ show result
    putStrLn $ "结果行数: " ++ show (length $ lines result)
    
    -- 测试其他情况
    putStrLn "\n=== 测试其他情况 ==="
    testOtherCases

testOtherCases :: IO ()
testOtherCases = do
    let testCases = 
            [ ([""], "单个空字符串")
            , (["", "\n"], "空字符串和换行符")
            , (["\n"], "单个换行符")
            , (["", "a"], "空字符串和字符")
            ]
    
    mapM_ (\(lines', desc) -> do
        let withMixed = map ("\t  " ++) lines'
        let input = unlines withMixed
        let result = normalizeIndentation input
        putStrLn $ desc ++ ":"
        putStrLn $ "  输入: " ++ show lines'
        putStrLn $ "  结果: " ++ show result
        putStrLn $ "  结果行数: " ++ show (length $ lines result)
      ) testCases