module Main where

import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing the actual failing case:"
  
  -- QuickCheck 生成的失败输入
  let s = "\""
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "s = " ++ show s
  putStrLn $ "withEscape = " ++ show withEscape
  putStrLn $ "U.isProblematicUnclosedString withEscape = " ++ show (U.isProblematicUnclosedString withEscape)
  
  -- 检查 withEscape 的值
  putStrLn "\nChecking withEscape character by character:"
  putStrLn $ "withEscape = " ++ show withEscape
  mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "]: " ++ show c ++ " (code: " ++ show (fromEnum c) ++ ")") (zip [0..] withEscape)