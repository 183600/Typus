import Utils

main :: IO ()
main = do
  let lineLen = 0
      content = ""
      safeContent = if null content then "a" else content
      longLine = replicate lineLen ' ' ++ safeContent ++ replicate lineLen ' '
      trimmed = trim longLine
      processed = removeLineComments longLine
  putStrLn $ "longLine: " ++ show longLine
  putStrLn $ "trimmed: " ++ show trimmed
  putStrLn $ "safeContent: " ++ show safeContent
  putStrLn $ "trimmed == safeContent: " ++ show (trimmed == safeContent)
  putStrLn $ "length processed <= length longLine: " ++ show (length processed <= length longLine)
