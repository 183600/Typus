import Utils

main :: IO ()
main = do
    let input = "\\"  -- 双引号 + 反斜杠
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Length: " ++ show (length input)
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)
    
    -- 手动检查特殊情况
    putStrLn "\nManual check:"
    case input of
      "\\" -> putStrLn "Matches special case, should be False"
      _ -> putStrLn "Does not match special case"