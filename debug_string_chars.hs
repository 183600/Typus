import qualified Utils as U

main :: IO ()
main = do
  let s = "a\""
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "Length of s: " ++ show (length s)
  putStrLn $ "Chars in s: " ++ show (zip [0..] s)
  
  let lastTwo = if length s >= 2 then drop (length s - 2) s else ""
  putStrLn $ "Last 2 chars: " ++ show lastTwo
  
  let endsWithEscapedQuote = not (null s) && length s >= 2 && drop (length s - 2) s == "\\\""
  putStrLn $ "endsWithEscapedQuote: " ++ show endsWithEscapedQuote
  
  -- 检查实际的最后两个字符
  if length s >= 2
    then do
      let c1 = s !! (length s - 2)
      let c2 = s !! (length s - 1)
      putStrLn $ "Last char: " ++ show c2 ++ " (ord: " ++ show (fromEnum c2) ++ ")"
      putStrLn $ "Second last char: " ++ show c1 ++ " (ord: " ++ show (fromEnum c1) ++ ")"
      putStrLn $ "Is last char '\"': " ++ show (c2 == '"')
      putStrLn $ "Is second last char '\\': " ++ show (c1 == '\\')
    else putStrLn "String too short"