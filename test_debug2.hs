import Utils

main :: IO ()
main = do
  let lineLen = 0
      content = " "
      safeContent = if null content then "a" else content
      longLine = replicate lineLen ' ' ++ safeContent ++ replicate lineLen ' '
      trimmed = trim longLine
      processed = removeLineComments longLine
  putStrLn $ "lineLen: " ++ show lineLen
  putStrLn $ "content: " ++ show content
  putStrLn $ "safeContent: " ++ show safeContent
  putStrLn $ "longLine: " ++ show longLine
  putStrLn $ "trimmed: " ++ show trimmed
  putStrLn $ "safeContent: " ++ show safeContent
  putStrLn $ "trimmed == safeContent: " ++ show (trimmed == safeContent)
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "length processed: " ++ show (length processed)
  putStrLn $ "length longLine: " ++ show (length longLine)
  putStrLn $ "length processed <= length longLine + 1: " ++ show (length processed <= length longLine + 1)
