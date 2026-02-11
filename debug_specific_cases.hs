import Utils

-- Test specific cases
main :: IO ()
main = do
  putStrLn $ "isProblematicUnclosedString \"a\": " ++ show (Utils.isProblematicUnclosedString "a")
  putStrLn $ "isProblematicUnclosedString \"b\\\"\": " ++ show (Utils.isProblematicUnclosedString "b\"")
  putStrLn $ "isProblematicUnclosedString \"c\": " ++ show (Utils.isProblematicUnclosedString "c")
  putStrLn $ "isProblematicUnclosedString \"\\\\\": " ++ show (Utils.isProblematicUnclosedString "\\")