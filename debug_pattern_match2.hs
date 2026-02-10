testPattern :: String -> String
testPattern s = 
  case s of
    "\\" -> "Matched \"\\\\ -> True\""
    "\\" -> "Matched \"\\\\ -> False\""
    "\\" -> "Matched \"\\\\ -> False\""
    _ -> "No match"

main :: IO ()
main = do
    let s = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "s: " ++ show s ++ " -> " ++ testPattern s