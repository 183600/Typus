import qualified Utils as U

main :: IO ()
main = do
  let s = "\"a\""
  putStrLn $ "String: " ++ show s
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)
  putStrLn $ "isProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString s)
  
  -- 检查是否以 " 结尾
  putStrLn $ "Ends with \": " ++ show (if length s >= 2 then drop (length s - 2) s == "\"" else False)