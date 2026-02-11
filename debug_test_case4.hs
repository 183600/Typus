module Main where

import qualified Utils as U

main :: IO ()
main = do
  let testStr = "\"\"\\\""
  putStrLn $ "Testing string: " ++ show testStr
  putStrLn $ "isCompleteStringLiteral testStr: " ++ show (U.isCompleteStringLiteral testStr)
  putStrLn $ "isProblematicUnclosedString testStr: " ++ show (U.isProblematicUnclosedString testStr)
  
  -- 重新编译并测试
  putStrLn "\nTesting after recompilation:"
  putStrLn $ "isProblematicUnclosedString \"\\\"\\\\\\\"\": " ++ show (U.isProblematicUnclosedString "\"\"\\\"")