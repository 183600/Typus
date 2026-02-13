main :: IO ()
main = do
  let s = "a"
      closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
  putStrLn $ "s = " ++ show s ++ " (length " ++ show (length s) ++ ")"
  putStrLn $ "closed = " ++ show closed ++ " (length " ++ show (length closed) ++ ")"
  putStrLn $ "unclosed = " ++ show unclosed ++ " (length " ++ show (length unclosed) ++ ")"