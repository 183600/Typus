import qualified Utils as U

main :: IO ()
main = do
  let s = "a\""
  putStrLn $ "Input s: " ++ show s
  
  let closed = "\"" ++ s ++ "\""
  putStrLn $ "closed: " ++ show closed
  putStrLn $ "isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
  
  let unclosed = "\"" ++ s
  putStrLn $ "unclosed: " ++ show unclosed
  putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
  
  -- 测试期望
  putStrLn $ "\nTest expects:"
  putStrLn $ "  closed should be False (not problematic)"
  putStrLn $ "  unclosed should be True (problematic)"