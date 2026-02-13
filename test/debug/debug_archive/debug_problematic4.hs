import Utils

main :: IO ()
main = do
  let s = ""
  putStrLn $ "s = " ++ show s
  putStrLn $ "null s = " ++ show (null s)
  putStrLn $ "isProblematicUnclosedString s = " ++ show (isProblematicUnclosedString s)