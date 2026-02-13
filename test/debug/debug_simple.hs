-- 简单的调试脚本
module Main where

-- 导入Utils模块
import Utils

main :: IO ()
main = do
    let testCases = ["\"", "%", "\SYN", "\SUB", "]", "#", "\1073968", " ", "+"]
    
    putStrLn "Testing isProblematicUnclosedString with escaped quotes:"
    mapM_ runTest testCases
  where
    runTest s = do
        let withEscape = "\"" ++ s ++ "\\\""
        let result = isProblematicUnclosedString withEscape
        putStrLn $ "Input: " ++ show s ++ ", WithEscape: " ++ show withEscape ++ ", Result: " ++ show result