import qualified Utils as U

main :: IO ()
main = do
  let s1 = ""
      s2 = ""
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  
  let complex = "\"" ++ s1 ++ "\\\"" ++ s2
  putStrLn $ "complex: " ++ show complex
  
  let result = U.isProblematicUnclosedString complex
  putStrLn $ "isProblematicUnclosedString complex: " ++ show result
  
  putStrLn $ "\nExpected: False"
  putStrLn $ "Test passes: " ++ show (not result)