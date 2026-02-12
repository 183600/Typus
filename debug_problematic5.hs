import Utils

main :: IO ()
main = do
  let s = "a"
      closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
      isProblematicClosed = isProblematicUnclosedString closed
      isProblematicUnclosed = isProblematicUnclosedString unclosed
  putStrLn $ "s = " ++ show s ++ " (length " ++ show (length s) ++ ")"
  putStrLn $ "closed = " ++ show closed ++ " (length " ++ show (length closed) ++ ")"
  putStrLn $ "unclosed = " ++ show unclosed ++ " (length " ++ show (length unclosed) ++ ")"
  putStrLn $ "isProblematicUnclosedString closed = " ++ show isProblematicClosed
  putStrLn $ "isProblematicUnclosedString unclosed = " ++ show isProblematicUnclosed
  putStrLn $ "Expected: closed=False, unclosed=True"