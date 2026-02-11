module Main where

import qualified Utils as U

main :: IO ()
main = do
  let testStr = "\"\"\\\""
  putStrLn $ "Testing string: " ++ show testStr
  putStrLn $ "isCompleteStringLiteral testStr: " ++ show (U.isCompleteStringLiteral testStr)
  putStrLn $ "isProblematicUnclosedString testStr: " ++ show (U.isProblematicUnclosedString testStr)
  
  -- 分析字符串
  putStrLn "\nString analysis:"
  mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "]: " ++ show c ++ " (code: " ++ show (fromEnum c) ++ ")") (zip [0..] testStr)