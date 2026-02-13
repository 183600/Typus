import qualified Utils as U

main :: IO ()
main = do
  let s = "\"aa\\\""
  putStrLn $ "String: " ++ show s
  putStrLn $ "Length: " ++ show (length s)
  putStrLn $ "Last 2 chars: " ++ show (drop (length s - 2) s)
  putStrLn $ "Compare with \\\": " ++ show (drop (length s - 2) s == "\\\"")
  
  putStrLn $ "\nisProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString s)
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)