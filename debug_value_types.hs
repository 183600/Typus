#!/usr/bin/env runhaskell

import qualified Ownership as Own

-- 测试值类型的所有权分析
main :: IO ()
main = do
    putStrLn "=== 调试值类型所有权问题 ==="
    
    let testCases = [
            ("x := 42; y := x; z := x", "整数赋值"),
            ("a := true; b := a; c := a", "布尔值赋值"),
            ("s := \"hello\"; t := s; u := s", "字符串赋值")
            ]
    
    mapM_ (\(code, desc) -> do
        putStrLn $ "\n--- " ++ desc ++ " ---"
        putStrLn $ "代码: " ++ code
        let errors = Own.analyzeOwnership code
        putStrLn $ "错误数量: " ++ show (length errors)
        putStrLn $ "错误详情: " ++ show errors
        putStrLn $ "应该为0个错误: " ++ if null errors then "✓ 正确" else "✗ 失败"
        ) testCases