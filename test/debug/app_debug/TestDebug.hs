module Main where

import Utils

-- 模拟测试用例
prop_is_problematic_unclosed_escape_quote_test :: String -> Bool
prop_is_problematic_unclosed_escape_quote_test s =
  let withEscape = "\"" ++ s ++ "\\\""
  in if s == ""
     then isProblematicUnclosedString "\""  -- 特殊情况：只有引号
     else if s == "\\"
          then isProblematicUnclosedString "\\"  -- 特殊情况：反斜杠
          else isProblematicUnclosedString withEscape

main :: IO ()
main = do
    let testCases = ["\"", "%", "\SYN", "\SUB", "]", "#", "\1073968", " ", "+"]
    
    putStrLn "Testing prop_is_problematic_unclosed_escape_quote logic:"
    mapM_ runTest testCases
  where
    runTest s = do
        let withEscape = "\"" ++ s ++ "\\\""
        let result = prop_is_problematic_unclosed_escape_quote_test s
        putStrLn $ "Input: " ++ show s ++ ", WithEscape: " ++ show withEscape ++ ", Result: " ++ show result