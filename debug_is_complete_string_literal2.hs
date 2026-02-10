import Utils

main :: IO ()
main = do
    let testCases = 
          [ ("\"\\\"", "\"\\\"")  -- 双引号 + 反斜杠 + 双引号
          , ("\"\\", "\"\\")      -- 双引号 + 反斜杠
          , ("\\\\", "\\\\")      -- 双反斜杠
          ]
    mapM_ (\(name, input) -> do
        putStrLn $ name ++ ": " ++ show input ++ " -> " ++ show (isCompleteStringLiteral input)
        ) testCases