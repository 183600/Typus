#!/usr/bin/env runhaskell

import qualified Ownership as Own

-- 调试词法分析器如何处理不同的值类型
main :: IO ()
main = do
    putStrLn "=== 调试词法分析器 ==="
    
    let testCases = [
            ("x := 42", "整数字面量"),
            ("x := true", "布尔值true"),
            ("x := false", "布尔值false"),
            ("x := \"hello\"", "字符串字面量"),
            ("x := y", "变量赋值")
            ]
    
    mapM_ (\(code, desc) -> do
        putStrLn $ "\n--- " ++ desc ++ " ---"
        putStrLn $ "代码: " ++ code
        let toks = Own.lexAll code
        putStrLn $ "词法单元: " ++ show toks
        let errors = Own.analyzeOwnership code
        putStrLn $ "所有权错误: " ++ show errors
        ) testCases