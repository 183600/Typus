import qualified Utils as U

main :: IO ()
main = do
  let s = "\"a\\\""
  putStrLn $ "String: " ++ show s
  putStrLn $ "Length: " ++ show (length s)
  putStrLn $ "Chars: " ++ show (map (\(i,c) -> (i, c)) $ zip [0..] s)
  
  putStrLn $ "\nPattern matching test:"
  let matches = case s of
                  ('"':_:'\\':'"':_) -> "Matches pattern"
                  _ -> "Does not match pattern"
  putStrLn $ matches
  
  putStrLn $ "\nisProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString s)
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)