import Utils

-- Test the exact test logic
main :: IO ()
main = do
  let s = "a\""
  let closed = "\"" ++ s ++ "\""
  let unclosed = "\"" ++ s
  putStrLn $ "s: " ++ show s
  putStrLn $ "closed: " ++ show closed
  putStrLn $ "unclosed: " ++ show unclosed
  putStrLn $ "isProblematicUnclosedString closed: " ++ show (Utils.isProblematicUnclosedString closed)
  putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (Utils.isProblematicUnclosedString unclosed)