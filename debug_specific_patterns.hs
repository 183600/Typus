import Utils

main :: IO ()
main = do
    putStrLn "=== Testing specific patterns ==="
    
    -- 测试所有特殊情况
    let testCases = 
            [ ("\"\\\\", "Double quote + double backslash")
            , ("\"a\\\\", "Double quote + a + double backslash")
            , ("\"\"\\\\", "Double quote + double quote + double backslash")
            , ("\"\\\\\"", "Double quote + double backslash + double quote")
            , ("\"a\\\\\"", "Double quote + a + double backslash + double quote")
            ]
    
    mapM_ (\(input, desc) -> do
        putStrLn $ "\n" ++ desc ++ ": " ++ show input
        putStrLn $ "  isCompleteStringLiteral: " ++ show (Utils.isCompleteStringLiteral input)
        putStrLn $ "  endsWithDoubleBackslash: " ++ show (endsWithDoubleBackslash input)
      ) testCases

-- 检查字符串是否以双反斜杠结尾
endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"