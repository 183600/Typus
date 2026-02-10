testPattern :: String -> String
testPattern s = 
  case s of
    "\\" -> "Matched \"\\\\ -> True\""
    "\\" -> "Matched \"\\\\ -> False\""
    _ -> "No match"

main :: IO ()
main = do
    let s1 = "\"" ++ "\\"  -- 双引号 + 反斜杠
    let s2 = "\\"  -- 反斜杠
    putStrLn $ "s1: " ++ show s1 ++ " -> " ++ testPattern s1
    putStrLn $ "s2: " ++ show s2 ++ " -> " ++ testPattern s2