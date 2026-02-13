import Utils

-- 模拟 normalizeIndentation 函数的逻辑，添加调试信息
testNormalizeIndentation :: String -> String
testNormalizeIndentation input =
  let result = normalizeIndentation input
  in if result == "\n"
     then result
     else if result == "    "
          then "    (from somewhere)"
          else "other: " ++ result

main :: IO ()
main = do
    let input = "\t  \n"
    putStrLn $ "Input: " ++ show input
    let result = normalizeIndentation input
    putStrLn $ "Actual result: " ++ show result
    putStrLn $ "Result length: " ++ show (length result)
    putStrLn $ "Result chars: " ++ show (map (\c -> (c, fromEnum c)) result)